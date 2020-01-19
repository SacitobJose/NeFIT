package dealer;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.AbstractMap.SimpleEntry;

import java.util.Comparator;

import protos.Protos.DealerTimeout;
import protos.Protos.Import;
import protos.Protos.Produce;
import protos.Protos.SaleInfo;
import protos.Protos.Transaction;

/**
 * Comparator for sorting the importers by (quantity * unitaryPrice)
 */
class ImportComparator implements Comparator<Import> {
    public int compare(Import i1, Import i2) {
        long total1 = i1.getQuantity() * i1.getUnitaryPrice();
        long total2 = i2.getQuantity() * i2.getUnitaryPrice();

        return total1 > total2 ? -1 : (total1 < total2 ? 1 : 0);
    }
}

/**
 * Dealer
 */
public class Dealer {
    // <ProducerName, ProductName> => [Import]
    public static HashMap<SimpleEntry<String, String>, ArrayList<Import>> negotiations = new HashMap<>();
    public static ArrayList<Import> unmatchImports = new ArrayList<>();

    public static void main(String[] args) {
        try {
            Socket socket = new Socket("localhost", 1234);
            System.out.println("Conectado!");
            BufferedReader is = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            PrintWriter os = new PrintWriter(socket.getOutputStream());

            while (true) {
                String line = is.readLine();
                System.out.println("Li do socket, nova mensagem\n");

                String[] parts = line.split(":");
                System.out.printf("%s %s\n", parts[0], parts[1]);

                Transaction transaction = Transaction.parseFrom(parts[1].getBytes());
                System.out.println("Tenho uma transaction\n");

                switch (transaction.getTxnCase()) {
                case PRODUCE:
                    System.out.println("Nova oferta de produtor\n");
                    // Produce
                    Produce produce = transaction.getProduce();
                    String producer = produce.getProducerName();
                    String product = produce.getProductName();
                    negotiations.put(new SimpleEntry<>(producer, product), new ArrayList<>());

                    for (Import i : unmatchImports) {
                        ArrayList<Import> importers = new ArrayList<>();
                        String producerI = i.getProducerName();
                        String productI = i.getProductName();

                        if (producerI.equals(producer) && productI.equals(product)) {
                            importers.add(i);
                        }

                        negotiations.put(new SimpleEntry<>(producer, product), importers);
                    }

                    Thread timeout = new TimeoutThread(negotiations, produce.getProductName(),
                            produce.getProducerName(), produce.getMinimumAmount(), produce.getMaximumAmount(),
                            produce.getMinimumUnitaryPrice(), produce.getNegotiationPeriod(), os);
                    timeout.start();

                    break;
                case IMPORT:
                    // Import
                    Import imp = transaction.getImport();

                    ArrayList<Import> orders = negotiations
                            .get(new SimpleEntry<>(imp.getProducerName(), imp.getProductName()));
                    if (orders != null)
                        orders.add(imp);
                    else
                        unmatchImports.add(imp);

                    break;
                default:
                    System.out.println("Não entrei no case !!!");
                    break;
                }
            }
        } catch (Exception e) {
            System.out.println("Não foi possível conectar ao negociador.");
        }
    }
}

/**
 * Timeout thread
 */
class TimeoutThread extends Thread {
    private HashMap<SimpleEntry<String, String>, ArrayList<Import>> negotiations;
    private String productName;
    private String producerName;
    private long minimumAmount;
    private long maximumAmount;
    private long minimumUnitaryPrice;
    private long time;
    private PrintWriter os;

    public TimeoutThread(HashMap<SimpleEntry<String, String>, ArrayList<Import>> negotiations, String productName,
            String producerName, long minimumAmount, long maximumAmount, long minimumUnitaryPrice, long sec,
            PrintWriter os) {
        this.negotiations = negotiations;
        this.productName = productName;
        this.producerName = producerName;
        this.minimumAmount = minimumAmount;
        this.maximumAmount = maximumAmount;
        this.minimumUnitaryPrice = minimumUnitaryPrice;
        this.time = sec;
        this.os = os;
    }

    public void run() {
        try {
            Thread.sleep(this.time * 1000);

            System.out.println("Timeout\n");

            // Time is up, wrap up the sale
            ArrayList<Import> importers = this.negotiations.get(new SimpleEntry<>(this.producerName, this.productName));

            /* ---------- CHOOSE THE BEST IMPORTERS TO MEET REQUIREMENTS ---------- */

            // Sorted "importers" by declared comparator
            ImportComparator ic = new ImportComparator();
            importers.sort(ic);

            // Get product quantity
            long total = 0;
            ArrayList<Integer> buyers = new ArrayList<>();
            for (Import imp : importers) {
                if (imp.getQuantity() + total < this.maximumAmount
                        && imp.getUnitaryPrice() >= this.minimumUnitaryPrice) {
                    total += imp.getQuantity();
                    buyers.add(importers.indexOf(imp));
                } else if (imp.getUnitaryPrice() < this.minimumUnitaryPrice) {
                    break;
                }
            }

            // Kill
            if (total < this.minimumAmount) {
                buyers.clear();
            }

            // Remove data from importers selected
            for (int index : buyers) {
                importers.remove(index);
            }

            synchronized (negotiations) {
                negotiations.put(new SimpleEntry<>(this.producerName, this.productName), importers);
            }

            /* -------------------------------------------------------------------- */

            // Send the response to server
            StringBuilder timeout = new StringBuilder();
            timeout.append("DealerTimeout:");

            DealerTimeout.Builder dealerTimeout = DealerTimeout.newBuilder();
            boolean success = buyers.size() > 0;
            dealerTimeout.setSuccess(success);
            timeout.append(String.valueOf(success));

            timeout.append(":");

            dealerTimeout.setProducerName(this.producerName);
            timeout.append(this.producerName);

            timeout.append(":");

            dealerTimeout.setProductName(this.productName);
            timeout.append(this.productName);

            for (int index : buyers) {
                Import import1 = importers.get(index);

                timeout.append(":");
                String username = import1.getProducerName();
                SaleInfo.Builder saleInfo = SaleInfo.newBuilder();
                saleInfo.setUsername(username);
                saleInfo.setQuantity(import1.getQuantity());
                saleInfo.setPrice(import1.getUnitaryPrice());

                dealerTimeout.addSales(saleInfo.build());
                timeout.append(username);
                timeout.append(":");
                timeout.append(saleInfo.build());
            }

            timeout.append(":");
            timeout.append(dealerTimeout.build());

            this.os.println(timeout.toString());
            this.os.flush();

            System.out.println("Resultado enviado para o servidor\n");

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

package dealer;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.AbstractMap.SimpleEntry;

import protos.Protos.DealerTimeout;
import protos.Protos.Import;
import protos.Protos.Produce;
import protos.Protos.SaleInfo;
import protos.Protos.Transaction;

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
            BufferedReader is = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            PrintWriter os = new PrintWriter(socket.getOutputStream());

            while (true) {
                String line = is.readLine();

                Transaction transaction = Transaction.parseFrom(line.getBytes());
                switch (transaction.getTxnCase()) {
                case PRODUCE: {
                    // Produce
                    Produce produce = transaction.getProduce();
                    negotiations.put(new SimpleEntry<>(produce.getProducerName(), produce.getProductName()),
                            new ArrayList<>());

                    Thread timeout = new TimeoutThread(produce.getNegotiationPeriod(), negotiations,
                            produce.getProducerName(), produce.getProductName(), os);
                    timeout.start();

                    break;
                }
                case IMPORT: {
                    // Import
                    Import imp = transaction.getImport();

                    ArrayList<Import> orders = negotiations
                            .get(new SimpleEntry<>(imp.getProducerName(), imp.getProductName()));
                    if (orders != null)
                        orders.add(imp);
                    else
                        unmatchImports.add(imp);

                    break;
                }
                default:
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
    private String producerName;
    private String productName;
    private long time;
    private PrintWriter os;

    public TimeoutThread(long sec, HashMap<SimpleEntry<String, String>, ArrayList<Import>> negotiations,
            String producerName, String productName, PrintWriter os) {
        this.negotiations = negotiations;
        this.producerName = producerName;
        this.productName = productName;
        this.time = sec;
        this.os = os;
    }

    public void run() {
        try {
            Thread.sleep(this.time * 1000);

            // Time is up, wrap up the sale
            ArrayList<Import> importers = this.negotiations.get(new SimpleEntry<>(this.producerName, this.productName));

            ///////////// CHOOSE THE BEST IMPORTERS TO MEET REQUIREMENTS //////////////

            DealerTimeout.Builder dealerTimeout = DealerTimeout.newBuilder();
            dealerTimeout.setSuccess(true);
            dealerTimeout.setProducerName(this.producerName);
            dealerTimeout.setProductName(this.productName);

            for (Import import1 : importers) {
                SaleInfo.Builder saleInfo = SaleInfo.newBuilder();
                saleInfo.setUsername(import1.getProducerName());
                saleInfo.setQuantity(import1.getQuantity());
                saleInfo.setPrice(import1.getUnitaryPrice());

                dealerTimeout.addSales(saleInfo);
            }

            os.println(dealerTimeout.build().toString());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

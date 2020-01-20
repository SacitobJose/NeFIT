package dealer;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.AbstractMap.SimpleEntry;

import protos.Protos.POSTNegotiation;
import protos.Protos.POSTNegotiationOver;
import protos.Protos.CatalogRequest;

import java.util.Comparator;

class Import {
    public String username;
    public String productName;
    public String producerName;
    public int quantity;
    public int unitaryPrice;

    public int getQuantity() {
        return quantity;
    }

    public int getUnitaryPrice() {
        return unitaryPrice;
    }

    public String getUsername() {
        return username;
    }

    public String getProducerName() {
        return producerName;
    }

    public String getProductName() {
        return productName;
    }

    public Import(String username, String productName, String producerName, int quantity, int unitaryPrice) {
        this.username = username;
        this.productName = productName;
        this.producerName = producerName;
        this.quantity = quantity;
        this.unitaryPrice = unitaryPrice;
    }

    public boolean equals(Object o) {
        Import i = (Import) o;
        return this.username.equals(i.getUsername())
                && this.productName.equals(i.getProductName())
                && this.producerName.equals(i.getProducerName())
                && this.quantity == i.getQuantity()
                && this.unitaryPrice == i.getUnitaryPrice();
    }
}

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
            BufferedReader is = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            PrintWriter os = new PrintWriter(socket.getOutputStream());

            while (true) {
                String line = is.readLine();
                String[] parts = line.split("_");
                if (parts[0].equals("Produce")) {
                    // Produce
                    String producer = parts[1];
                    String product = parts[2];
                    int minimumQuantity = Integer.parseInt(parts[3]);
                    int maximumQuantity = Integer.parseInt(parts[4]);
                    int minimumPrice = Integer.parseInt(parts[5]);
                    int negotiationPeriod = Integer.parseInt(parts[6]);
                    ArrayList<Import> arr = new ArrayList<>();
                    negotiations.put(new SimpleEntry<>(producer, product), arr);

                    for (Import i : unmatchImports) {
                        String producerI = i.getProducerName();
                        String productI = i.getProductName();

                        if (producerI.equals(producer) && productI.equals(product)) {
                            arr.add(i);
                            unmatchImports.remove(i);
                        }
                    }

                    Thread timeout = new TimeoutThread(negotiations, unmatchImports, product, producer, minimumQuantity,
                            maximumQuantity, minimumPrice, negotiationPeriod, os);
                    timeout.start();

                    POSTNegotiation.Builder pn = POSTNegotiation.newBuilder();
                    pn.setProducerName(producer);
                    pn.setProductName(product);
                    pn.setMinimumAmount(minimumQuantity);
                    pn.setMaximumAmount(maximumQuantity);
                    pn.setMinimumUnitaryPrice(minimumPrice);
                    pn.setNegotiationPeriod(negotiationPeriod);

                    Socket catalog = new Socket("localhost", 9999);
                    CatalogRequest.Builder cr = CatalogRequest.newBuilder();
                    cr.setNn(pn.build());
                    cr.build().writeDelimitedTo(catalog.getOutputStream());
                } else {
                    // Import
                    String importer = parts[1];
                    String product = parts[2];
                    String producer = parts[3];
                    int quantity = Integer.parseInt(parts[4]);
                    int pricePerProduct = Integer.parseInt(parts[5]);

                    ArrayList<Import> orders = negotiations.get(new SimpleEntry<>(producer, product));
                    if (orders != null)
                        orders.add(new Import(importer, product, producer, quantity, pricePerProduct));
                    else
                        unmatchImports.add(new Import(importer, product, producer, quantity, pricePerProduct));
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
    public ArrayList<Import> unmatchImports = new ArrayList<>();
    private String productName;
    private String producerName;
    private long minimumAmount;
    private long maximumAmount;
    private long minimumUnitaryPrice;
    private long time;
    private PrintWriter os;

    public TimeoutThread(HashMap<SimpleEntry<String, String>, ArrayList<Import>> negotiations,
            ArrayList<Import> unmatchImports, String productName, String producerName, long minimumAmount,
            long maximumAmount, long minimumUnitaryPrice, long sec, PrintWriter os) {
        this.negotiations = negotiations;
        this.productName = productName;
        this.producerName = producerName;
        this.minimumAmount = minimumAmount;
        this.maximumAmount = maximumAmount;
        this.minimumUnitaryPrice = minimumUnitaryPrice;
        this.time = sec;
        this.os = os;
        this.unmatchImports = unmatchImports;
    }

    public void run() {
        try {
            Thread.sleep(this.time * 1000);

            Socket catalog = new Socket("localhost", 9999);
            POSTNegotiationOver.Builder no = POSTNegotiationOver.newBuilder();
            no.setProductName(this.productName);
            no.setProducerName(this.producerName);
            CatalogRequest.Builder cr = CatalogRequest.newBuilder();
            cr.setNo(no);
            cr.build().writeDelimitedTo(catalog.getOutputStream());

            // Time is up, wrap up the sale
            ArrayList<Import> importers = this.negotiations.get(new SimpleEntry<>(this.producerName, this.productName));

            /* ---------- CHOOSE THE BEST IMPORTERS TO MEET REQUIREMENTS ---------- */

            // Sorted "importers" by declared comparator
            ImportComparator ic = new ImportComparator();
            importers.sort(ic);

            // Get product quantity
            long total = 0;
            ArrayList<Import> buyers = new ArrayList<>();
            for (Import imp : importers) {
                System.out.println(imp.getUsername());
                if (imp.getQuantity() + total <= this.maximumAmount
                        && imp.getUnitaryPrice() >= this.minimumUnitaryPrice) {
                    total += imp.getQuantity();
                    buyers.add(imp);
                } else if (imp.getUnitaryPrice() < this.minimumUnitaryPrice) {
                    break;
                }
            }

            if (total < this.minimumAmount) {
                buyers.clear();
            }

            /* -------------------------------------------------------------------- */

            // Send the response to server
            StringBuilder timeout = new StringBuilder();
            timeout.append("DealerTimeout_");

            timeout.append(String.valueOf(buyers.size() > 0));
            timeout.append("_");

            timeout.append(this.producerName);
            timeout.append("_");

            timeout.append(this.productName);

            for (Import import1 : buyers) {
                importers.remove(import1);

                timeout.append("_");

                timeout.append(import1.getUsername());
                timeout.append("_");

                timeout.append(import1.getQuantity());
                timeout.append("_");

                timeout.append(import1.getUnitaryPrice());
            }

            synchronized (negotiations) {
                synchronized (unmatchImports) {
                    negotiations.remove(new SimpleEntry<>(this.producerName, this.productName));
                    unmatchImports.addAll(importers);
                }
            }

            this.os.println(timeout);
            this.os.flush();
        } catch (

        Exception e) {
            e.printStackTrace();
        }
    }
}

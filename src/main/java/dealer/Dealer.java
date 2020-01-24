package dealer;

import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.AbstractMap.SimpleEntry;

import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMsg;

import protos.Protos.POSTNegotiation;
import protos.Protos.Produce;
import protos.Protos.SaleInfo;
import protos.Protos.Transaction;
import protos.Protos.DELETENegotiation;
import protos.Protos.DealerTimeout;
import protos.Protos;
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

    @Override
    public boolean equals(Object o) {
        if (o == null)
            return false;
        if (o == this)
            return true;
        if (!(o instanceof Import))
            return false;
        Import i = (Import) o;
        return this.username.equals(i.getUsername()) && this.productName.equals(i.getProductName())
                && this.producerName.equals(i.getProducerName()) && this.quantity == i.getQuantity()
                && this.unitaryPrice == i.getUnitaryPrice();
    }

    @Override
    public int hashCode() {
        return this.username.hashCode();
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
    public static void main(String[] args) {
        try {
            Socket socket = new Socket("localhost", 1234);
            InputStream is = socket.getInputStream();
            OutputStream os = socket.getOutputStream();

            ZContext ctx = new ZContext();
            org.zeromq.ZMQ.Socket publisher = ctx.createSocket(ZMQ.PUB);
            publisher.connect("tcp://localhost:7777");

            ZMsg zmsg = new ZMsg();

            HashMap<SimpleEntry<String, String>, ArrayList<Import>> negotiations = new HashMap<>();
            ArrayList<Import> unmatchImports = new ArrayList<>();

            while (true) {
                byte[] header = is.readNBytes(4);
                int size = ByteBuffer.wrap(header).getInt();
                byte[] data = is.readNBytes(size);

                Transaction trans = Transaction.parseFrom(data);
                switch (trans.getTxnCase()) {
                case IMPORT: {
                    // Import
                    Protos.Import imp = trans.getImport();

                    String importer = imp.getImporterName();
                    String product = imp.getProductName();
                    String producer = imp.getProducerName();
                    int quantity = (int) imp.getQuantity();
                    int pricePerProduct = (int) imp.getUnitaryPrice();

                    ArrayList<Import> orders = negotiations.get(new SimpleEntry<>(producer, product));
                    if (orders != null)
                        orders.add(new Import(importer, product, producer, quantity, pricePerProduct));
                    else
                        unmatchImports.add(new Import(importer, product, producer, quantity, pricePerProduct));
                    break;
                }

                case PRODUCE: {
                    // Produce
                    Produce prod = trans.getProduce();

                    String producer = prod.getProducerName();
                    String product = prod.getProductName();
                    int minimumQuantity = (int) prod.getMinimumAmount();
                    int maximumQuantity = (int) prod.getMaximumAmount();
                    int minimumPrice = (int) prod.getMinimumUnitaryPrice();
                    int negotiationPeriod = (int) prod.getNegotiationPeriod();
                    
                    ArrayList<Import> arr = new ArrayList<>();
                    negotiations.put(new SimpleEntry<>(producer, product), arr);

                    ArrayList<Import> newUnmatchImports = new ArrayList<>();

                    for (int i = unmatchImports.size() - 1; i >= 0; i--) {
                        Import imp = unmatchImports.get(i);
                        String producerI = imp.getProducerName();
                        String productI = imp.getProductName();

                        if (producerI.equals(producer) && productI.equals(product)) {
                            arr.add(imp);
                        } else {
                            newUnmatchImports.add(imp);
                        }
                    }
                    unmatchImports = newUnmatchImports;

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

                    zmsg.newStringMsg(producer + product);
                    zmsg.send(publisher);
                    break;
                }
                default:
                    break;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
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
    private OutputStream os;

    public TimeoutThread(HashMap<SimpleEntry<String, String>, ArrayList<Import>> negotiations,
            ArrayList<Import> unmatchImports, String productName, String producerName, long minimumAmount,
            long maximumAmount, long minimumUnitaryPrice, long sec, OutputStream os) {
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
            DELETENegotiation.Builder no = DELETENegotiation.newBuilder();
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
            DealerTimeout.Builder timeout = DealerTimeout.newBuilder();

            timeout.setSuccess(buyers.size() > 0);

            timeout.setProducerName(this.producerName);

            timeout.setProductName(this.productName);

            for (Import import1 : buyers) {
                importers.remove(import1);

                SaleInfo.Builder si = SaleInfo.newBuilder();

                si.setUsername(import1.getUsername());

                si.setQuantity(import1.getQuantity());

                si.setPrice(import1.getUnitaryPrice());

                timeout.addSales(si.build());
            }

            synchronized (negotiations) {
                synchronized (unmatchImports) {
                    negotiations.remove(new SimpleEntry<>(this.producerName, this.productName));
                    unmatchImports.addAll(importers);
                }
            }

            DealerTimeout dt = timeout.build();
            byte[] size = ByteBuffer.allocate(4).putInt(dt.getSerializedSize()).array();
            this.os.write(size);
            dt.writeTo(os);
            this.os.flush();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

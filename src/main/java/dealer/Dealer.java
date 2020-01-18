package dealer;

import protos.Protos.Transaction;
import protos.Protos.Produce;
import protos.Protos.Import;

import protos.Protos.SaleInfo;
import protos.Protos.DealerTimeout;

// import java.io.BufferedReader;
// import java.io.InputStream;
// import java.io.InputStreamReader;
// import java.io.OutputStream;
import java.net.Socket;

/**
 * Dealer
 */
public class Dealer {
    public static void main(String[] args) {
        try {
            int port = Integer.parseInt(args[1]);
            Socket s = new Socket("localhost", port);
            Thread dts = new DealerToSocket(s);
            Thread timeout = new TimeoutThread(dts, 10000);
            dts.start();
            timeout.start();
        } catch (Exception e) {
            System.out.println("Não foi possível conectar ao negociador.");
        }
    }
}

/**
 * Timeout thread
 */
class TimeoutThread extends Thread {
    private Thread t;
    private int time;

    public TimeoutThread(Thread t, int mills) {
        this.t = t;
        this.time = mills;
    }

    public void run() {
        try {
            Thread.sleep(this.time);
            this.t.notify();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

/**
 * DealerToSocket
 */
class DealerToSocket extends Thread {
    Socket socket;

    public DealerToSocket(Socket s) {
        this.socket = s;
    }

    public void run() {
        try {
            // InputStream is = this.socket.getInputStream();
            // OutputStream os = this.socket.getOutputStream();
            // BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in));

            while (true) {
                SaleInfo.Builder saleInfo;
                DealerTimeout.Builder dealerTimeout;

                // Handle request
                Transaction.Builder transaction = Transaction.newBuilder();
                switch (transaction.getTxnCase()) {
                case PRODUCE:
                    // Produce
                    Produce produce = transaction.getProduce();

                    // Handle response
                    saleInfo = SaleInfo.newBuilder();
                    dealerTimeout = DealerTimeout.newBuilder();

                    // ...

                    break;

                case IMPORT:
                    // Import
                    Import imp = transaction.getImport();

                    // Handle response
                    saleInfo = SaleInfo.newBuilder();
                    dealerTimeout = DealerTimeout.newBuilder();

                    saleInfo.setUsername(imp.getProducerName());
                    saleInfo.setQuantity(imp.getQuantity());
                    saleInfo.setPrice(imp.getUnitaryPrice());

                    dealerTimeout.setSuccess(true);
                    dealerTimeout.setProducerName(imp.getProducerName());
                    dealerTimeout.setProductName(imp.getProductName());
                    dealerTimeout.addSales(saleInfo);

                    break;

                default:
                    // DO SOMETHING ?
                    break;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
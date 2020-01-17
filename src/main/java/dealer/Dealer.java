package dealer;

import protos.Protos.Produce;
import protos.Protos.Import;

import protos.Protos.SaleInfo;
import protos.Protos.DealerTimeout;

import protos.Protos.ServerResponse;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
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
            dts.start();
        } catch (Exception e) {
            System.out.println("Não foi possível conectar ao negociador.");
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
            InputStream is = this.socket.getInputStream();
            OutputStream os = this.socket.getOutputStream();
            BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in));

            // Receber pedidos durante x tempo
            // Enviar resposta ao servidor com a informação correta
            while (true) {
                // Handle request
                Produce.Builder produce = Produce.newBuilder();
                Import.Builder imp = Import.newBuilder();

                // Handle response
                // DealerTimeout & SaleInfo declarations
                DealerTimeout.Builder dealerTimeout = DealerTimeout.newBuilder();
                SaleInfo.Builder saleInfo = SaleInfo.newBuilder();

                dealerTimeout.setSuccess(true);
                dealerTimeout.setProducerName(imp.getProducerName());
                dealerTimeout.setProductName(produce.getProductName());

                saleInfo.setUsername(imp.getProducerName());
                saleInfo.setQuantity(imp.getQuantity());
                saleInfo.setPrice(imp.getUnitaryPrice());

                dealerTimeout.addSales(saleInfo);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
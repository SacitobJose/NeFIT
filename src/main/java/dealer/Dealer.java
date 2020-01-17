package dealer;

import protos.Protos.SaleInfoOrBuilder;
import protos.Protos.ServerResponse;
import protos.Protos.DealerTimeoutOrBuilder;

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

            while (true) {
                // Get server request
                ServerResponse aVal = ServerResponse.parseDelimitedFrom(is);

                // DealerTimeout & SaleInfo declarations
                DealerTimeoutOrBuilder.Builder dealerTimeout = DealerTimeoutOrBuilder.newBuilder();
                SaleInfoOrBuilder saleInfo = SaleInfoOrBuilder.newBuilder();

                dealer.setSuccess(aVal.getSuccess());
                dealer.setProducerName(aVal.getProducerName());
                dealer.setProductName(aVal.getProductName());

                dealer.addAllSales();

                // Receber pedidos durante x tempo
                // Enviar resposta ao servidor com a informação correta
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
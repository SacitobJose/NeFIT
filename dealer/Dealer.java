package dealer;

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
            Socket s = new Socket("localhost", 8000);
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
                // Receber pedidos durante x tempo
                // Enviar resposta ao servidor com a informação correta
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

}
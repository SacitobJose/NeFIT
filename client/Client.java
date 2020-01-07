package client;

import client.Protos.*;
import client.Protos.Authentication.Builder;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.Socket;

public class Client {
    public static void main(String[] args) {
        try {
            Socket s = new Socket("localhost", 1234);
            Thread cts = new ClientToSocket(s);
            cts.start();
        } catch (Exception e) {
            System.out.println("Não foi possível conectar ao servidor");
        }
    }
}

class ClientToSocket extends Thread {
    Socket socket;
    String username;

    public ClientToSocket(Socket cli) throws IOException {
        this.socket = cli;
        this.username = null;
    }

    public void run() {
        try {
            InputStream is = this.socket.getInputStream();
            OutputStream os = this.socket.getOutputStream();
            BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in));

            while (true) {
                // Verificar o papel do utilizador
                System.out.print("É um (f)abricante ou um (i)mportador? ");
                String role = stdin.readLine();
                if (!role.equals("f") && !role.equals("i")) {
                    System.out.println("Não é nenhum dos papéis válidos");
                    continue;
                }

                // Verificar credenciais do utilizador
                System.out.print("Deseja fazer (l)ogin ou (r)egistar-se? ");
                String method = stdin.readLine();
                Builder auth = Authentication.newBuilder();
                if (method.equals("r")) {
                    auth.setType(Authentication.AuthType.REGISTAR);
                } else if (method.equals("l")) {
                    auth.setType(Authentication.AuthType.LOGIN);
                } else {
                    System.out.println("Não é nenhum dos métodos válidos");
                    continue;
                }

                System.out.print("Username: ");
                String username = stdin.readLine();
                auth.setUsername(username);

                System.out.print("Password: ");
                auth.setPassword(stdin.readLine());

                // Try to authenticate
                auth.build().writeDelimitedTo(os);

                // Receive authentication confirmation
                ServerResponse aVal = ServerResponse.parseDelimitedFrom(is);
                if (!aVal.getSuccess()) {
                    if (method.equals("l"))
                        System.out.println("Username does not exist or password incorrect");
                    else
                        System.out.println("Username already exists");
                    continue;
                }

                System.out.println("Authentication successful");

                this.username = username;

                break;
            }

            // Iniciar thread para ler do socket para o terminal
            Thread stc = new SocketToClient(is);
            stc.start();

            // Iniciar diálogo com o utilizador
            while (true) {
                String st = stdin.readLine();
                if (st == null)
                    return;
                Message.newBuilder().setUsername(this.username).setContent(st).build().writeDelimitedTo(os);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

class SocketToClient extends Thread {
    InputStream is;

    public SocketToClient(InputStream is) throws IOException {
        this.is = is;
    }

    public void run() {
        try {
            while (true) {
                Message msg = Message.parseDelimitedFrom(is);
                System.out.println(msg.getUsername() + ": " + msg.getContent());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

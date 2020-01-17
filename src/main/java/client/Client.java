package client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.Socket;

import org.zeromq.ZMQ;

import protos.Protos.Authentication;
import protos.Protos.ServerResponse;

public class Client {
    public static void main(String[] args) {
        try {
            Socket s = new Socket("localhost", 1234);
            Thread cts = new ClientToSocket(s);
            cts.start();
        } catch (Exception e) {
            System.out.println("Não foi possível conectar ao servidor.");
        }
    }
}

class ClientToSocket extends Thread {
    Socket socket;
    String username;
    BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in));

    public ClientToSocket(Socket cli) throws IOException {
        this.socket = cli;
        this.username = null;
    }

    private void clearTerminal() {
        System.out.print("\033[H\033[2J");
        System.out.flush();
    }

    private void waitConfirmation() throws IOException {
        System.out.print("\nClique no ENTER para continuar...");
        System.out.flush();
        this.stdin.readLine();
    }

    private int readInt() throws IOException {
        while (true) {
            try {
                return Integer.parseInt(this.stdin.readLine());
            } catch (NumberFormatException exc) {
                System.out.println("Por favor, introduza um número");
            }
        }
    }

    private void importerMenu() throws IOException {
        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket subSocket = context.socket(ZMQ.SUB);

        while (true) {
            StringBuilder main = new StringBuilder();
            main.append("O que queres fazer?\n");
            main.append("1 - Oferta de encomenda\n");
            main.append("2 - Subscrever notificações\n");
            main.append("3 - Cancelar notificações\n");
            main.append("4 - Exit\n");

            clearTerminal();
            System.out.println(main);

            int escolha;
            do {
                escolha = readInt();
            } while (escolha < 1 || escolha > 4);

            switch (escolha) {
            case 1:
                break;
            case 2:
                break;
            case 3:
                break;
            case 4:
                clearTerminal();
                System.exit(1);
                break;
            }
            waitConfirmation();
        }
    }

    private void producerMenu() throws IOException {
        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket pubSocket = context.socket(ZMQ.PUB);

        while (true) {
            StringBuilder main = new StringBuilder();
            main.append("O que queres fazer?\n");
            main.append("1 - Oferta de produção\n");
            main.append("2 - Exit\n");

            clearTerminal();
            System.out.println(main);

            int escolha;
            do {
                escolha = readInt();
            } while (escolha < 1 || escolha > 2);

            switch (escolha) {
            case 1:
                break;
            case 2:
                clearTerminal();
                System.exit(1);
                break;
            }
            waitConfirmation();
        }
    }

    public void run() {
        try {
            InputStream is = this.socket.getInputStream();
            OutputStream os = this.socket.getOutputStream();
            String role;

            while (true) {
                Authentication.Builder auth = Authentication.newBuilder();

                // Verificar o papel do utilizador
                System.out.print("É um (f)abricante ou um (i)mportador? ");
                role = stdin.readLine();
                if (role.equals("f"))
                    auth.setUserType(Authentication.UserType.PRODUCER);
                else if (role.equals("i"))
                    auth.setUserType(Authentication.UserType.IMPORTER);
                else {
                    System.out.println("Não é nenhum dos papéis válidos.");
                    continue;
                }

                // Verificar credenciais do utilizador
                System.out.print("Deseja fazer (l)ogin ou (r)egistar-se? ");
                String method = stdin.readLine();
                if (method.equals("r"))
                    auth.setType(Authentication.AuthType.REGISTER);
                else if (method.equals("l"))
                    auth.setType(Authentication.AuthType.LOGIN);
                else {
                    System.out.println("Não é nenhum dos métodos válidos.");
                    continue;
                }

                System.out.print("Nome de utilizador: ");
                String username = stdin.readLine();
                auth.setUsername(username);

                System.out.print("Palavra-passe: ");
                auth.setPassword(stdin.readLine());

                // Try to authenticate
                auth.build().writeDelimitedTo(os);

                // Receive authentication confirmation
                ServerResponse aVal = ServerResponse.parseDelimitedFrom(is);
                if (!aVal.getSuccess()) {
                    if (method.equals("l"))
                        System.out.println("O nome do utilizador não existe ou a palavra passe está incorreta.");
                    else
                        System.out.println("O nome do utilizador já existe.");
                    continue;
                }

                System.out.println("Autenticação bem sucedida.");

                this.username = username;

                break;
            }

            waitConfirmation();

            // Iniciar thread para ler do socket para o terminal
            Thread stc = new SocketToClient(is);
            stc.start();

            if (role.equals("f"))
                producerMenu();
            else
                importerMenu();
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
                // DO SOMETHING
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

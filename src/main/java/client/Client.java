package client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

import org.zeromq.ZMQ;
import org.zeromq.ZContext;
import org.zeromq.SocketType;

import protos.Protos.DealerTimeout;
import protos.Protos.Import;
import protos.Protos.Produce;
import protos.Protos.SaleInfo;
import protos.Protos.Transaction;

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
    BufferedReader is;
    PrintWriter os;
    String username;
    BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in));

    public ClientToSocket(Socket cli) throws IOException {
        this.socket = cli;
        is = new BufferedReader(new InputStreamReader(cli.getInputStream()));
        os = new PrintWriter(cli.getOutputStream());
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
        ZContext context = new ZContext();
        ZMQ.Socket subSocket = context.createSocket(SocketType.SUB);
        subSocket.connect("tcp://localhost:7777");

        Thread s = new Subscriptions(subSocket);
        s.start();

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
                StringBuilder transaction = new StringBuilder();
                transaction.append("Transaction:");

                Transaction.Builder txn = Transaction.newBuilder();
                Import.Builder prod = Import.newBuilder();

                System.out.print("Nome do produto: ");
                System.out.flush();
                String productName = this.stdin.readLine();
                prod.setProductName(productName);
                transaction.append(productName);

                transaction.append(":");

                System.out.print("Nome de produtor: ");
                System.out.flush();
                prod.setProducerName(this.stdin.readLine());

                System.out.print("Quantidade: ");
                System.out.flush();
                prod.setQuantity(this.readInt());

                System.out.print("Preço por produto: ");
                System.out.flush();
                prod.setUnitaryPrice(this.readInt());

                txn.setImport(prod.build());

                transaction.append(txn.build());
                break;
            case 2: {
                System.out.print("Fabricante: ");
                System.out.flush();

                String producer = this.stdin.readLine();
                subSocket.subscribe(producer);
                break;
            }
            case 3: {
                System.out.print("Fabricante: ");
                System.out.flush();

                String producer = this.stdin.readLine();
                subSocket.unsubscribe(producer);
                break;
            }
            case 4:
                clearTerminal();
                context.close();
                System.exit(1);
                break;
            }
            waitConfirmation();
        }
    }

    private void producerMenu() throws IOException {
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
                StringBuilder transaction = new StringBuilder();
                transaction.append("Transaction:");

                Transaction.Builder txn = Transaction.newBuilder();
                Produce.Builder prod = Produce.newBuilder();

                System.out.print("Nome do produto: ");
                System.out.flush();
                String productName = this.stdin.readLine();
                prod.setProductName(productName);
                transaction.append(productName);

                transaction.append(":");

                System.out.print("Quantidade mínima: ");
                System.out.flush();
                prod.setMinimumAmount(this.readInt());

                System.out.print("Quantidade máxima: ");
                System.out.flush();
                prod.setMaximumAmount(this.readInt());

                System.out.print("Preço mínimo por produto: ");
                System.out.flush();
                prod.setMinimumUnitaryPrice(this.readInt());

                System.out.print("Período de negociação (segundos): ");
                System.out.flush();
                prod.setNegotiationPeriod(this.readInt());

                txn.setProduce(prod.build());

                transaction.append(txn.build());

                this.os.println(transaction.toString());
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
            String role;

            while (true) {
                StringBuilder auth = new StringBuilder();
                auth.append("Authentication:");

                System.out.print("Deseja fazer (l)ogin ou (r)egistar-se? ");
                System.out.flush();
                String method = stdin.readLine();
                if (method.equals("r"))
                    auth.append(1);
                else if (method.equals("l"))
                    auth.append(0);
                else {
                    System.out.println("Não é nenhum dos métodos válidos.");
                    continue;
                }

                auth.append(":");

                System.out.print("É um (f)abricante ou um (i)mportador? ");
                System.out.flush();
                role = stdin.readLine();
                if (role.equals("f"))
                    auth.append(0);
                else if (role.equals("i"))
                    auth.append(1);
                else {
                    System.out.println("Não é nenhum dos papéis válidos.");
                    continue;
                }

                auth.append(":");

                System.out.print("Nome de utilizador: ");
                System.out.flush();
                String username = stdin.readLine();
                auth.append(username);

                auth.append(":");

                System.out.print("Palavra-passe: ");
                System.out.flush();
                auth.append(stdin.readLine());

                // Try to authenticate
                System.out.println(auth.toString());
                this.os.println(auth.toString());
                this.os.flush();

                // Receive authentication confirmation
                String response = this.is.readLine();
                String[] res = response.split(":");
                if (res[1].equals("0")) {
                    if (method.equals("l"))
                        System.out.println("O nome de utilizador não existe ou a palavra passe está incorreta.");
                    else
                        System.out.println("O nome de utilizador já existe.");
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
    BufferedReader is;

    public SocketToClient(BufferedReader is) throws IOException {
        this.is = is;
    }

    public void run() {
        try {
            while (true) {
                String line = this.is.readLine();
                String[] parts = line.split(":");
                if (parts[0].equals("Import")) {
                    String producerName = parts[1];
                    String productName = parts[2];
                    SaleInfo sale = SaleInfo.parseFrom(parts[3].getBytes());
                    long quantity = sale.getQuantity();
                    long price = sale.getPrice();

                    System.out.println("Encomenda efetuada:");
                    System.out.println("\tProduto: " + Long.toString(quantity) + " " + productName);
                    System.out.println("\tPreço por unidade: " + Long.toString(price));
                    System.out.println("\tProdutor: " + producerName);
                } else {
                    DealerTimeout timeout = DealerTimeout.parseFrom(parts[1].getBytes());
                    String productName = timeout.getProductName();
                    if (timeout.getSuccess()) {
                        System.out.println("Venda efetuada:");
                        System.out.println("\tProduto: " + productName);
                        System.out.println("\tCompradores:");
                        timeout.getSalesList().forEach((sale) -> {
                            System.out.println("\t\t" + sale.getUsername() + ": " + Long.toString(sale.getQuantity()) + "x a " + Long.toString(sale.getPrice()) + "/unidade");
                        });
                    } else {
                        System.out.println("Venda de " + productName + "recusada");
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

class Subscriptions extends Thread {
    ZMQ.Socket subSocket;

    public Subscriptions(ZMQ.Socket subSocket) throws IOException {
        this.subSocket = subSocket;
    }

    public void run() {
        while (true) {
            byte[] b = this.subSocket.recv();
            System.out.println(new String(b));
        }
    }
}

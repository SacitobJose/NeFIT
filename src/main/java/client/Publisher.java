package client;

import org.zeromq.ZMQ;

public class Publisher {
    public static void main(String[] args) {
        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket socket = context.socket(ZMQ.PUB);
        socket.connect("tcp://localhost:" + args[0]);
        while (true) {
          String s = System.console().readLine();
          if (s == null) break;
          socket.send(s);
        }
        socket.close();
        context.term();
    }
}

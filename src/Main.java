import java.net.InetSocketAddress;
import com.sun.net.httpserver.HttpServer;

import service.TestService;

public class Main {
  public static void main(String[] args) throws Exception {
      HttpServer server = HttpServer.create(new InetSocketAddress(8000), 0);
      server.createContext("/test", new TestService());
      server.setExecutor(null); // creates a default executor
      server.start();
  }
}

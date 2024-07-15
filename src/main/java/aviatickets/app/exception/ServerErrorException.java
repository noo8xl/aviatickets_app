package aviatickets.app.exception;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
public class ServerErrorException extends RuntimeException {
  public ServerErrorException() {
    super("Internal server error..");
    this.sendMessage();
  }

  public ServerErrorException(String msg) {
    super(msg);
    this.sendMessage();
  }

  private void sendMessage() {
    // do some here to send notif
    // to the support service (to the developer?)
    // -> and save log to the file ?*
    // will updated soon
  }
}

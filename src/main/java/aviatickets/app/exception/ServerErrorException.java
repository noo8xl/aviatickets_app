package aviatickets.app.exception;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
public class ServerErrorException extends RuntimeException {
  public ServerErrorException() {
    super("Internal server error..");
    // do some here to send notif 
    // to the support service (to the developer?) 
    // -> and save log to the file ?* 
  }

  public ServerErrorException(String msg) {
    super(msg);
    // do some here to send notif 
    // to the support service (to the developer?) 
    // -> and save log to the file ?* 
  }
}

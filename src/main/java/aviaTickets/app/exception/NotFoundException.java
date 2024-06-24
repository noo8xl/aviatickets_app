package aviaTickets.app.exception;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.NOT_FOUND)
public class NotFoundException extends RuntimeException {
  public NotFoundException() {
    super("Data not found.");
  }

  public NotFoundException(String msg) {
    super(msg);
  }
}

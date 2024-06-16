package aviaTickets.app.exception;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.FORBIDDEN)
public class PermissionDeniedExceprion extends RuntimeException {
  public PermissionDeniedExceprion() {
    super("Rejected. Permission denied!");
  }
}

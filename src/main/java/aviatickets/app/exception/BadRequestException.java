package aviatickets.app.exception;


import aviatickets.app.notification.NotificationInterface;
import aviatickets.app.notification.NotificationService;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.BAD_REQUEST)
public class BadRequestException extends RuntimeException {

	public BadRequestException() {
			super("Bad request.");
		}

  public BadRequestException(String msg) {
    super(msg);
		NotificationInterface notification = new NotificationService();
		notification.sendErrorMessage(msg);
	}
}

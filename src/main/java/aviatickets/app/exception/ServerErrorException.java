package aviatickets.app.exception;

import aviatickets.app.notification.NotificationInterface;
import aviatickets.app.notification.NotificationService;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
public class ServerErrorException extends RuntimeException {

	private final static String DEFAULT_MESSAGE = "Internal server error.";
	private final NotificationInterface notification = new NotificationService();

	public ServerErrorException(String area) {
		super(DEFAULT_MESSAGE);
		this.notification.sendErrorMessage(DEFAULT_MESSAGE + area);
	}

}

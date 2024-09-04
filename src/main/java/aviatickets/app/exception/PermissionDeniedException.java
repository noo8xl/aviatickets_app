package aviatickets.app.exception;

import aviatickets.app.notification.NotificationInterface;
import aviatickets.app.notification.NotificationService;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.FORBIDDEN)
public class PermissionDeniedException extends RuntimeException {

	private final static String DEFAULT_MESSAGE = "Rejected. Permission denied!";
	private final static NotificationInterface notification = new NotificationService();

	public PermissionDeniedException(String area) {
		super(DEFAULT_MESSAGE);
		notification.sendErrorMessage(DEFAULT_MESSAGE + area);
	}
}

package aviatickets.app.exception;

import aviatickets.app.notification.NotificationInterface;
import aviatickets.app.notification.NotificationService;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.FORBIDDEN)
public class PermissionDeniedException extends RuntimeException {

	public PermissionDeniedException() {
		super("Rejected. Permission denied!");
		NotificationInterface notification = new NotificationService();
		notification.sendErrorMessage("Rejected. Permission denied!");
	}
}

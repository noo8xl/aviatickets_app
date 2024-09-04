package aviatickets.app.notification.dto;

public record NotificationDto(
		String type, // email or telegram
		String data, // code or password
		String recipient // email str or telegram id
) {
}

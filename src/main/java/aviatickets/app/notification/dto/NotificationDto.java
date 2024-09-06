package aviatickets.app.notification.dto;

import jakarta.validation.constraints.NotEmpty;

public record NotificationDto(
		@NotEmpty
		String type,
		@NotEmpty// email or telegram
		String data,
		@NotEmpty// code or password
		String recipient // email str or telegram id
) {
}

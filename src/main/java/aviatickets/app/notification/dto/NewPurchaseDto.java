package aviatickets.app.notification.dto;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;
import jdk.jfr.Timestamp;

import java.sql.Date;

public record NewPurchaseDto(
		@Email
		@NotEmpty
		String email,
		@NotEmpty
		String customerName,
		@Timestamp
		Date currentDate,
		@NotEmpty
		@Positive
		Integer purchaseId
) {
}

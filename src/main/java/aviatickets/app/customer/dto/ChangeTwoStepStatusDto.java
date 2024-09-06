package aviatickets.app.customer.dto;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;
import jdk.jfr.BooleanFlag;

public record ChangeTwoStepStatusDto(
		@Positive
		Integer customerId,
		@NotEmpty
		String type,
		String telegramId,// email or telegram
		@NotEmpty
		@Email
		String email,
		@NotEmpty
		@BooleanFlag
		Boolean status
) {
}

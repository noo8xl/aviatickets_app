package aviatickets.app.customer.dto;

import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;

public record ChangeTwoStepStatusDto(
		@Positive
		Integer customerId,
		@NotEmpty
		String email,
		@NotEmpty
		Boolean status
) {
}

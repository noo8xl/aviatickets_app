package aviatickets.app.purchase.dto.request;

import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;

public record CreatePurchaseDto(
	Integer id,
	@Positive
	@NotEmpty
	Short quantity,
	@Positive
	@NotEmpty
	Integer customerId,
	@Positive
	@NotEmpty
	Integer flightNumber
) {}

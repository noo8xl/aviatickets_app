package aviatickets.app.purchase.dto.request;

import jakarta.validation.constraints.Positive;
import jdk.jfr.BooleanFlag;
import lombok.NonNull;

public record UpdatePurchaseDto(
		@Positive
		@NonNull
		Integer id,
		@NonNull
		String flightNumber,
		@Positive
		@NonNull
		Short quantity,
		@Positive
		@NonNull
		Float price,
		@BooleanFlag
		@NonNull
		Boolean paymentStatus
) {}

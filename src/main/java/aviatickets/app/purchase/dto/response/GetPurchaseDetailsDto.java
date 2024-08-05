package aviatickets.app.purchase.dto.response;

import aviatickets.app.auth.dto.response.SignInResponse;
import aviatickets.app.flight.dto.response.DetailedFlightItemDto;
import aviatickets.app.purchase.entity.Purchase;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;

public record GetPurchaseDetailsDto(
		@Positive
		Integer id,
		@NotEmpty
		Purchase purchase,
		@NotEmpty
		SignInResponse customerInfo,
		@Positive
		@NotEmpty
		Float price,

		@NotEmpty
		DetailedFlightItemDto flightDetails

) {
}

package aviatickets.app.flight.entity;

import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.List;


@Getter
@NoArgsConstructor
public class FlightsItem {
	@Positive
	Integer id;
	@NotEmpty
	String flightNumber;
	@NotEmpty
	String airline;

	// Itinerary with Transfer: the legs of the journey are detailed,
	// showing the transfer at some Airport.
	@Setter
//	@Getter
//	@NotEmpty
	List<Leg> itinerary;
//	@Getter
	@Setter
//	@NotEmpty
	Aircraft aircraft;

	@Setter
//	@Getter
	@Positive
	Short totalDistance; // => leg distance += leg distance
//	@NotEmpty
	String totalDuration;
//	@NotEmpty
	Price price;
	@Positive
	Short passengerCount;
	@Positive
	Short availableSits;


	public void setFlightItem(
			Integer id, String flightNumber, String airline, Short totalDistance,
			String totalDuration, Short passengerCount, Short availableSits ) {
		this.id = id;
		this.flightNumber = flightNumber;
		this.airline = airline;
		this.totalDistance = totalDistance;
		this.totalDuration = totalDuration;
		this.passengerCount = passengerCount;
		this.availableSits = availableSits;
	}

}





//public record FlightsItem(
//		@Positive
//		Integer id,
//		@NotEmpty
//		String flightNumber,
//		@NotEmpty
//		String airline,
//
//		// Itinerary with Transfer: the legs of the journey are detailed,
//		// showing the transfer at some Airport.
//		@NotEmpty
//		List<Leg> itinerary,
//		@NotEmpty
//		Aircraft aircraft,
//
//		@Positive
//		Short totalDistance, // => leg distance += leg distance
//		@NotEmpty
//		String totalDuration,
//		@NotEmpty
//		Price price,
//		@Positive
//		Short passengerCount,
//		@Positive
//		Short availableSits
//) {
//
//}

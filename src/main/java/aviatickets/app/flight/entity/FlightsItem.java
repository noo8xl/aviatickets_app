package aviatickets.app.flight.entity;

import com.fasterxml.jackson.annotation.JsonIgnore;
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
	private Integer id;
	@NotEmpty
	private String flightNumber;
	@NotEmpty
	private String airline;

	// Itinerary with Transfer: the legs of the journey are detailed,
	// showing the transfer at some Airport.
	@Setter
	private List<Leg> itinerary;
	@Setter
	private Aircraft aircraft;

	@Positive
	private Short totalDistance; // => leg distance += leg distance
	@NotEmpty
	private String totalDuration;

	@Setter
	private Price price;

	@Positive
	private Short passengerCount;
	@Positive
	@Setter
	private Short availableSits;


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

	@JsonIgnore
	public FlightsItem getFlightItem() {
		return this;
	}

}


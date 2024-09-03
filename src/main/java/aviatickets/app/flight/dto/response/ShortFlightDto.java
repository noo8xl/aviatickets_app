package aviatickets.app.flight.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.sql.Date;

@Setter
@NoArgsConstructor
public class ShortFlightDto {
	@Getter
	private String flightNumber;

	private String departureAirportName;
	private String departureAirportCode;
	private String departureAirportCity;

	private String arrivalAirportName;
	private String arrivalAirportCode;
	private String arrivalAirportCity;

	private String duration;
	private Date departureDate;
	private Float price;

	@JsonIgnore
	public ShortFlightDto getShortFlightDto() {
		return this;
	}

}
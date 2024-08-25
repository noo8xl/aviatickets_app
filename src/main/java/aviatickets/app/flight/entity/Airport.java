package aviatickets.app.flight.entity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@NoArgsConstructor
public class Airport {
	@Positive
	private Integer id;
	@NotEmpty
	private String code;
	@NotEmpty
	private String airportName;
	@NotEmpty
	private String city;
	@NotEmpty
	private String country;
	@NotEmpty
	private String terminal;
	@NotEmpty
	private String timezone;
	@NotEmpty
	@Setter
	private Location location;

	@NotEmpty
	@Setter
	private AirportContacts contacts;

	public void setAirport(
			Integer id, String code, String airportName, String city,
			String country, String terminal, String timezone
	) {
		this.id = id;
		this.code = code;
		this.airportName = airportName;
		this.city = city;
		this.country = country;
		this.terminal = terminal;
		this.timezone = timezone;
	}

	@JsonIgnore
	public Airport getAirport() {
		return this;
	}

}
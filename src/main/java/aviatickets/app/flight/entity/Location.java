package aviatickets.app.flight.entity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.validation.constraints.NotEmpty;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Getter
@NoArgsConstructor
public class Location implements Serializable {
	@NotEmpty
	private String longitude;
	@NotEmpty
	private String latitude;
	@NotEmpty
	private String altitude;

	public void setLocation(String longitude, String latitude, String altitude) {
		this.longitude = longitude;
		this.latitude = latitude;
		this.altitude = altitude;
	}

	@JsonIgnore
	public Location getLocation() {
		return this;
	}
}
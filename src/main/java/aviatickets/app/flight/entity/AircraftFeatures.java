package aviatickets.app.flight.entity;


import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import jdk.jfr.BooleanFlag;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.io.Serializable;

@Getter
@NoArgsConstructor
public class AircraftFeatures implements Serializable {
	@Positive
	private Integer id;
	@BooleanFlag
	@NotNull
	private Boolean wifi;
	@BooleanFlag
	@NotNull
	private Boolean inFlightEntertainment;
	@BooleanFlag
	@NotNull
	private Boolean powerOutlets;
	@Setter
	private CabinClass cabinClass; // "Economy", "Business", "First"

	public void setAircraftFeatures(
			Integer id, Boolean wifi, Boolean inFlightEntertainment, Boolean powerOutlets
	) {
		this.id = id;
		this.wifi = wifi;
		this.inFlightEntertainment = inFlightEntertainment;
		this.powerOutlets = powerOutlets;
	}

	@JsonIgnore
	public AircraftFeatures getAircraftFeatures() {
		return this;
	}

}
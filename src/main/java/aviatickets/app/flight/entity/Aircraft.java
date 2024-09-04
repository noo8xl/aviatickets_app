package aviatickets.app.flight.entity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.io.Serializable;

@Getter
@NoArgsConstructor
public class Aircraft implements Serializable {

	@Positive
	private Integer id;
	@NotBlank
	private String model;
	@NotEmpty
	private String registration;
	@Positive
	private Short seatingCapacity;
	@Positive
	private Short yearOfManufacture;
	@Setter
	private AircraftFeatures features;

	public void setAircraft(
			Integer id, String model, String registration,
			Short seatingCapacity, Short yearOfManufacture
	){
		this.id = id;
		this.model = model;
		this.registration = registration;
		this.seatingCapacity = seatingCapacity;
		this.yearOfManufacture = yearOfManufacture;
	}

	@JsonIgnore
	public Aircraft getAircraft() {
		return this;
	}


}
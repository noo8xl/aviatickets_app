package aviatickets.app.flight.entity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;
import jdk.jfr.Timestamp;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.io.Serializable;
import java.sql.Date;


@Getter
@NoArgsConstructor
public class Leg implements Serializable {
	@Positive
	@NotEmpty
	private Integer id;
	@NotEmpty
	private Short legNumber;

	@Setter
	private Airport departureAirport;
	@Setter
	private Airport arrivalAirport;

	@NotEmpty
	@Timestamp
	private Date departureTime;
	@NotEmpty
	@Timestamp
	private Date arrivalTime;
	@NotEmpty
	private String duration;
	@Positive
	@NotEmpty
	private Short distance;
	@NotEmpty
	private String status;

	public void setLeg(
			Integer id, Short legNumber, Date departureTime, Date arrivalTime,
			String duration, Short distance, String status
	) {
		this.id = id;
		this.legNumber = legNumber;
		this.departureTime = departureTime;
		this.arrivalTime = arrivalTime;
		this.duration = duration;
		this.distance = distance;
		this.status = status;
	}

	@JsonIgnore
	public Leg getLegItem(){
		return this;
	}


}
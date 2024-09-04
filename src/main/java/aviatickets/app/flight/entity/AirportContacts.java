package aviatickets.app.flight.entity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.validation.constraints.NotEmpty;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Getter
@NoArgsConstructor
public class AirportContacts implements Serializable {
	@NotEmpty
	private String phone;
	@NotEmpty
	private String email;
	@NotEmpty
	private String website;

	public void setAirportContacts(String phone, String email, String website) {
		this.phone = phone;
		this.email = email;
		this.website = website;
	}

	@JsonIgnore
	public AirportContacts getAirportContacts() {
		return this;
	}

}
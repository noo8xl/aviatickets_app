package aviatickets.app.flight.entity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.validation.constraints.NotEmpty;
import jdk.jfr.BooleanFlag;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class CabinClass {

	@BooleanFlag
	@NotEmpty
	Boolean economy;
	@BooleanFlag
	@NotEmpty
	Boolean business;
	@BooleanFlag
	@NotEmpty
	Boolean first;

	public void setCabinClass(
			Boolean economy, Boolean business, Boolean first) {
		this.economy = economy;
		this.business = business;
		this.first = first;
	}

	@JsonIgnore
	public CabinClass getCabinClass() {
		return this;
	}
}
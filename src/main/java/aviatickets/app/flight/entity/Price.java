package aviatickets.app.flight.entity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class Price {
	@NotEmpty
	String currency;
	@Positive
	Float amount;
	@Positive
	Short discount;
	@NotEmpty
	String baggageAllowance;


	public void setPrice(String currency, Float amount, Short discount, String baggageAllowance) {
		this.currency = currency;
		this.amount = amount;
		this.discount = discount;
		this.baggageAllowance = baggageAllowance;
	}

	@JsonIgnore
	public Price getPrice() {
		return this;
	}
}
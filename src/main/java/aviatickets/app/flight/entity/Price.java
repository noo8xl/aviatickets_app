package aviatickets.app.flight.entity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class Price {
	@Positive
	private Integer id;
	@NotEmpty
	private String flightNumber;
	@NotEmpty
	private String currency;
	@Positive
	private Float amount;
	@Positive
	private Short discount;
	@NotEmpty
	private String baggageAllowance;


	public void setPrice(
			Integer id, String flightNumber, String currency,
			Float amount, Short discount, String baggageAllowance)
	{
		this.id = id;
		this.flightNumber = flightNumber;
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
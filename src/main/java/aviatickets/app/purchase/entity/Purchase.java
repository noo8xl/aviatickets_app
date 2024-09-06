package aviatickets.app.purchase.entity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.hibernate.validator.constraints.Length;

import java.sql.Date;

@Getter
@NoArgsConstructor
public class Purchase {

	@Getter
	@Positive
	private Integer id = null;
	@NotEmpty
	private String flightNumber;
	@Getter
	@Positive
	@NotEmpty
	private Integer customerId;

	@Positive
	@Length(min = 1, max = 250)
	private Short quantity;
	@Positive
	private Float price;
	private Date createdAt = new Date(System.currentTimeMillis());
	private Date updatedAt = new Date(System.currentTimeMillis());
	@Getter
	private Boolean paymentStatus;

	public void setPurchase(
			Integer id, String flightNumber, Integer customerId, Short quantity,
			Float price, Date createdAt, Date updatedAt, Boolean paymentStatus
	) {
		if (Boolean.FALSE.equals(id == null))
			this.id = id;

		this.flightNumber = flightNumber;
		this.customerId = customerId;
		this.quantity = quantity;
		this.price = price;

		if (Boolean.FALSE.equals(createdAt == null))
			this.createdAt = createdAt;

		if (Boolean.FALSE.equals(updatedAt == null))
			this.updatedAt = updatedAt;

		this.paymentStatus = paymentStatus;
	}

	@JsonIgnore
	public Purchase getPurchase() {
		return this;
	}

}
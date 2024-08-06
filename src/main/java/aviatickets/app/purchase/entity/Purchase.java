package aviatickets.app.purchase.entity;

import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;
import lombok.Getter;
import lombok.Setter;

import java.sql.Date;

@Setter
public class Purchase {

	@Getter
	@Positive
	@NotEmpty
	private Integer id = null;
	private String flightNumber;
	private Integer customerId;

	private Short quantity;
	private Float price;
	private Date createdAt = new Date(System.currentTimeMillis());
	private Date updatedAt = new Date(System.currentTimeMillis());
	@Getter
	private Boolean paymentStatus;

	public Purchase() {}


	public void setPurchase(
			Integer id, String flightNumber, Integer customerId, Short quantity,
			Float price, Date createdAt, Date updatedAt, Boolean paymentStatus
	) {
		this.id = id;
		this.flightNumber = flightNumber;
		this.customerId = customerId;
		this.quantity = quantity;
		this.price = price;
		this.createdAt = createdAt;
		this.updatedAt = updatedAt;
		this.paymentStatus = paymentStatus;
	}

	public Purchase getPurchase() {
		return this;
	}

}
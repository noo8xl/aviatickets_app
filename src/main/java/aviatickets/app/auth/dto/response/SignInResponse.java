package aviatickets.app.auth.dto.response;

import aviatickets.app.customer.entity.Customer;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;
import org.hibernate.validator.constraints.Length;

public class SignInResponse {
	// Token t,
	@Positive
	private Integer customerId;
	@NotEmpty
	@Length(min = 4, max = 28)
	private String name;
	@NotEmpty
	@Email
	private String email;
	@NotEmpty
	private String role;
	private Boolean isBanned;
	private Boolean twoStepStatus;

	private String token;

	public SignInResponse(Customer c, String token) {
		this.customerId = c.getCustomerId();
		this.email = c.getUsername();
		this.isBanned = c.getBanStatus();
		this.twoStepStatus = c.get2faStatus();
		this.role = c.getAuthorities().toString();
		this.name = c.getCustomerName();
		this.token = token;
	}

	public SignInResponse getResponse() {
		return this;
	}
}

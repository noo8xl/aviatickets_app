package aviatickets.app.actions.entity;

import java.sql.Date;
import jdk.jfr.Timestamp;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;


public class ActionLog {
	@Positive
	private Integer id;
	@NotEmpty
	@Email
	private String email;

	@Timestamp
	private Date date = new Date(System.currentTimeMillis());

	@NotEmpty
	private String action;
	@Positive
	private Integer customerId;

	public ActionLog setAction(Integer id, String email, Date date, String action, Integer customerId) {
		this.id = id;
		this.email = email;
		this.action = action;
		this.customerId = customerId;

		if(Boolean.TRUE.equals(date != null)) {
			this.date = date;
		}

		return this;
	}

	public String getCustomerEmail(){
		return this.email;
	}

	public String getCustomerAction(){
		return this.action;
	}

	public Integer getCustomerId(){
		return this.customerId;
	}

}
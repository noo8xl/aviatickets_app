package aviatickets.app.actions.entity;

import java.sql.Date;
import jdk.jfr.Timestamp;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;
import lombok.Getter;
import lombok.NoArgsConstructor;


@NoArgsConstructor
public class ActionLog {
	@Positive
	private Integer id;
	@Getter
	@NotEmpty
	@Email
	private String email;

	@Timestamp
	private Date date = new Date(System.currentTimeMillis());

	@Getter
	@NotEmpty
	private String action;
	@Getter
	@Positive
	private Integer customerId;

public void setAction(Integer id, String email, Date date, String action, Integer customerId) {
		this.id = id;
		this.email = email;
		this.action = action;
		this.customerId = customerId;

		if(Boolean.TRUE.equals(date != null)) {
			this.date = date;
		}
	}

	public ActionLog getActionEntity() {
		return this;
	}

}
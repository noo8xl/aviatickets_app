package aviatickets.app.customer.entity;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;
import org.hibernate.validator.constraints.Length;
import jdk.jfr.Timestamp;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import java.sql.Date;
import java.util.Collection;
import java.util.List;

public class Customer implements UserDetails {
	@Positive
	private Integer id;
	@NotEmpty
	@Length(min = 4, max = 28)
	private String name;
	@NotEmpty
	@Email
	private String email;
	@NotEmpty
	@Length(min = 8, max = 30)
	private String password;
	@Timestamp
	private Date createdAt = new Date(System.currentTimeMillis());
	@NotEmpty
	private Role role = Role.USER;
	private Boolean isBanned = false;
	private Boolean twoStepStatus = false;

	public Customer(
			Integer id, String name, String email, String password,
			Date createdAt, Role role, Boolean isBanned, Boolean twoStepStatus) {
		this.id = id;
		this.name = name;
		this.email = email;
		this.password = password;
		this.createdAt = createdAt;

		if(role != null) {
			this.role = role;
		}
		if(isBanned != null) {
			this.isBanned = isBanned;
		}
		if(twoStepStatus != null) {
			this.twoStepStatus = twoStepStatus;
		}
	}

	@Override
	public Collection<? extends GrantedAuthority> getAuthorities() {
		return List.of(new SimpleGrantedAuthority(this.role.name()));
	}

	// ############################# getters area #####################################

	@Override
	public String getPassword() {
		return this.password;
	}

	@Override
	public String getUsername() {
		return this.email;
	}

//	@Override
	public String getCustomerName() {
		return this.name;
	}

	public Integer getCustomerId() {
		return this.id;
	}

	public Boolean get2faStatus() {
		return this.twoStepStatus;
	}

	public Boolean getBanStatus() {
		return this.isBanned;
	}




	// ########################### end of getters area ##################################


	@Override
	public boolean isAccountNonExpired() {
		return true;
	}

	@Override
	public boolean isAccountNonLocked() {
		return true;
	}

	@Override
	public boolean isCredentialsNonExpired() {
		return true;
	}

	@Override
	public boolean isEnabled() {
		return true;
	}
}

package aviatickets.app.customer.entity;

import java.sql.Date;
import java.util.Collection;
import java.util.List;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;
import org.hibernate.validator.constraints.Length;
import jdk.jfr.Timestamp;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;


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
	private Role role = Role.ADMIN;
	private Boolean isBanned = false;
	private Boolean twoStepStatus = false;

	public Customer(){}


	public Customer setCustomer(
			Integer id, String name, String email, String password,
			Date createdAt, Role role, Boolean isBanned, Boolean twoStepStatus
	) {
		this.id = id;
		this.name = name;
		this.email = email;
		this.password = password;

		if (Boolean.TRUE.equals(createdAt != null)) {
			this.createdAt = createdAt;
		}

		if (Boolean.TRUE.equals(twoStepStatus != null)) {
			this.twoStepStatus = twoStepStatus;
		}

		if (Boolean.TRUE.equals(role != null)) {
			this.role = role;
		}

		if (Boolean.TRUE.equals(isBanned != null)) {
			this.isBanned = isBanned;
		}

		return this;
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

//	@Override
//	public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
//		if(this.role.equals(Role.USER)) {
//			List<GrantedAuthority> grantedAuthorities = new ArrayList<>();
//			grantedAuthorities.add(new SimpleGrantedAuthority(this.role.name()));
//		} else if(this.role.equals(Role.ADMIN)) {
//
//		} else {
//			throw new UsernameNotFoundException(username);
//		}
//	}
//
//	private Customer buildCustomerObj() {
//		Customer c = new Customer();
//		c.id = this.id;
//		c.name = this.name;
//		c.email = this.email;
//		c.isBanned = this.isBanned;
//		c.twoStepStatus = this.twoStepStatus;
//
//		System.out.println("Customer set this -> " + c.toString());
//		return c;
//	}



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

package aviatickets.app.customer;

import java.sql.Date;
import java.sql.SQLException;
import java.util.List;
import java.util.Objects;

import aviatickets.app.actions.ActionService;
import aviatickets.app.actions.entity.ActionLog;
import aviatickets.app.customer.dto.ChangeTwoStepStatusDto;
import aviatickets.app.customer.dto.UpdateCustomerDto;
import aviatickets.app.customer.entity.Role;
import aviatickets.app.email.EmailService;
import aviatickets.app.exception.BadRequestException;
import aviatickets.app.exception.NotFoundException;
import aviatickets.app.exception.PermissionDeniedException;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import aviatickets.app.customer.entity.Customer;
import aviatickets.app.exception.ServerErrorException;

@Service
public class CustomerService implements CustomerInteraction {

//  private static final Logger log = LoggerFactory.getLogger(CustomerService.class);
  private final CustomerRepository customerRepository;
	private final EmailService emailService;
	private final ActionService actionService;

	public CustomerService(CustomerRepository customerRepository, EmailService emailService, ActionService actionService) {
		this.customerRepository = customerRepository;
		this.emailService = emailService;
		this.actionService = actionService;
	}

	@Override
  public void isCustomerExists(String email) throws BadRequestException, SQLException, ClassNotFoundException {
    Customer c = this.getCustomer(email);
		if (Boolean.FALSE.equals((c != null))) {
			throw new NotFoundException("Customer with email '" + email + "' not found.");
		}
	}

	@Override
	public void isCustomerExists(Integer id) throws BadRequestException, SQLException, ClassNotFoundException {
		Customer c = this.getCustomer(id);
		if (Boolean.FALSE.equals((c != null))) {
			throw new NotFoundException("Customer with id '" + id + "' not found.");
		}
	}

	@Override
  public void createCustomer(String name, String password, String email) throws SQLException, ClassNotFoundException {

		Customer c = this.customerRepository.findOne(email);
		if (Boolean.TRUE.equals((c != null))) {
			throw new BadRequestException("Bad request. Email already taken.");
		}

    this.customerRepository.save(name, email, password);
  }

	@Override
	@Cacheable("customer")
	public Customer getCustomer(Integer id) throws SQLException, ClassNotFoundException {
		return this.customerRepository.findOne(id);
	}

	@Override
	@Cacheable("customer")
  public Customer getCustomer(String email) throws SQLException, ClassNotFoundException {
    return this.customerRepository.findOne(email);
  }

	@Override
  public void updateProfile(UpdateCustomerDto dto) throws SQLException, ClassNotFoundException {
		this.isCustomerExists(dto.email());
		this.customerRepository.update(dto);
  }

	@Override
  public Integer changePassword(String email, String pwd) throws ServerErrorException, SQLException, ClassNotFoundException {
		this.isCustomerExists(email);
		return this.customerRepository.updatePassword(email, pwd);
  }

	@Override
  public void deleteCustomer(Integer idToDelete, Integer adminId) throws SQLException, ClassNotFoundException {
		this.customerRepository.delete(idToDelete, adminId);
  }

	@Override
	public void change2faStatus(ChangeTwoStepStatusDto dto) throws SQLException, ClassNotFoundException {
		ActionLog a = new ActionLog(
				null,
				dto.email(),
				new Date(System.currentTimeMillis()),
				"Customer 2fa status was changed! Current status is: " + dto.status() + ".",
				dto.customerId()
		);

		this.customerRepository.update2faStatus(dto);
		this.emailService.sendTwoStepCode(dto.email());
		this.actionService.saveCustomerAction(a);
	}

	@Override
	public List<Customer> getAll(Integer skip, Integer limit, Integer adminId) throws SQLException, ClassNotFoundException {
		this.checkCustomerPermission(adminId);
		return this.customerRepository.findAll(skip, limit);
	}

	@Override
	public void changeBanStatus(Integer customerId, Boolean status, Integer adminId) throws SQLException, ClassNotFoundException {
		this.checkCustomerPermission(adminId);
		this.customerRepository.updateIsBannedStatus(customerId, status);
	}


	@Override
	public Boolean getTwoStepStatus(String email) throws SQLException, ClassNotFoundException {
		return this.customerRepository.getTwoStepStatus(email);
	}

	// ###########################################################################

	private void checkCustomerPermission(Integer id) throws SQLException, ClassNotFoundException {
		Customer c = this.customerRepository.findOne(id);

		System.out.println("customer role is -> " + c.getAuthorities());

		if(c.getAuthorities().equals(Role.ADMIN)) {
			throw new PermissionDeniedException();
		}
	}
}

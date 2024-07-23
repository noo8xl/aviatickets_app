package aviatickets.app.customer;

import java.sql.SQLException;
import java.time.LocalDateTime;
import java.util.Date;
import java.util.List;

import aviatickets.app.actions.ActionService;
import aviatickets.app.actions.entity.ActionLog;
import aviatickets.app.customer.dto.ChangeTwoStepStatusDto;
import aviatickets.app.email.EmailService;
import aviatickets.app.exception.BadRequestException;
import aviatickets.app.exception.NotFoundException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import aviatickets.app.customer.entity.Customer;
import aviatickets.app.customer.entity.Role;
import aviatickets.app.exception.ServerErrorException;

@Service
public class CustomerService implements CustomerInteraction {

  private static final Logger log = LoggerFactory.getLogger(CustomerService.class);
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
		if (c == null) {
			throw new NotFoundException("Customer with email '" + email + "' not found.");
		}
		// should be updated
	}

  public void createCustomer(String name, String password, String email) throws SQLException, ClassNotFoundException {

		Customer c = this.getCustomer(email);
		if (c != null) {
			throw new BadRequestException("Bad request. Email already taken.");
		}
    customerRepository.save(name, email, password);
  }

  @Cacheable(key = "#id", value = "customer")
  public Customer getCustomer(Integer id) throws SQLException, ClassNotFoundException {
    return customerRepository.findOne(id);
  }

  public Customer getCustomer(String email) throws SQLException, ClassNotFoundException {
    return customerRepository.findOne(email);
  }

  public List<Customer> getAll(Integer skip, Integer limit) throws SQLException, ClassNotFoundException {
    return customerRepository.findAll(skip, limit);
  }

  public void updateProfile(Customer c) throws SQLException, ClassNotFoundException {
		this.isCustomerExists(c.email());
    customerRepository.update(c);
  }

  public Integer changePassword(String email, String pwd) throws ServerErrorException, SQLException, ClassNotFoundException {
		this.isCustomerExists(email);
//		Optional<Customer> c = getCustomer(email);
//
//    if (c.isPresent()) {
//      Customer updated = new Customer(null, c.get().name(), email, pwd, c.get().createdAt(), new Date(),
//          c.get().isBanned(), c.get().role());
//      customerRepository.update(updated, c.get().id());
//      return c.get().id();
//    }
//    throw new ServerErrorException();
		return 0;
  }

  public void deleteCustomer(Integer idToDelete, Integer customerId) throws SQLException, ClassNotFoundException {
    customerRepository.delete(customerId, idToDelete);
  }

	public void change2faStatus(ChangeTwoStepStatusDto dto) throws SQLException, ClassNotFoundException {
		ActionLog a = new ActionLog(
				null,
				dto.email(),
				LocalDateTime.now(),
				"Customer 2fa status was changed! Current status is: " + dto.status() + ".",
				dto.customerId()
		);

		customerRepository.update2faStatus(dto);
		emailService.sendTwoStepCode(dto.email());
		actionService.saveCustomerAction(a);
	}


	public Boolean getTwoStepStatus(String email) throws SQLException, ClassNotFoundException {
		return customerRepository.getTwoStepStatus(email);
	}
}

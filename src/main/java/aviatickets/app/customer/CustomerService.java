package aviatickets.app.customer;

import java.sql.SQLException;
import java.time.LocalDateTime;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import aviatickets.app.actions.ActionService;
import aviatickets.app.actions.entity.ActionLog;
import aviatickets.app.customer.dto.ChangeTwoStepStatusDto;
import aviatickets.app.email.EmailService;
import aviatickets.app.exception.BadRequestException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import aviatickets.app.customer.entity.Customer;
import aviatickets.app.customer.entity.Role;
import aviatickets.app.exception.PermissionDeniedException;
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

  public void isCustomerExists(String email) throws BadRequestException {
    Customer c = getCustomer(email);
		// should be updated
	}

  public void createCustomer(String name, String password, String email) {

		Customer c = this.getCustomer(email);
		if (c != null) {
			throw new BadRequestException("Bad request. Email already taken.");
		}

    Customer customer = new Customer(
        null,
        name,
        email,
        password,
        new Date(),
        new Date(),
        false,
        Role.USER);

    log.info("customer -> {}", customer);
    customerRepository.save(customer);
  }

  @Cacheable(key = "#id", value = "customer")
  public Customer getCustomer(Integer id) {
    return customerRepository.findOne(id);
  }

  public Customer getCustomer(String email) {
    return customerRepository.findOne(email);
  }

  public List<Customer> getAll(Short skip, Short limit) {
    return customerRepository.findAll();
  }

  public void updateProfile(Integer id, Customer c) {
		this.isCustomerExists(c.email());
    customerRepository.update(c, id);
  }

  public Integer changePassword(String email, String pwd) throws ServerErrorException {
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

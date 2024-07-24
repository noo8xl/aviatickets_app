package aviatickets.app.customer;

import java.sql.Date;
import java.sql.SQLException;
import java.util.List;

import aviatickets.app.actions.ActionService;
import aviatickets.app.actions.entity.ActionLog;
import aviatickets.app.customer.dto.ChangeTwoStepStatusDto;
import aviatickets.app.email.EmailService;
import aviatickets.app.exception.BadRequestException;
import aviatickets.app.exception.NotFoundException;
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
		// should be updated
	}

	@Override
	public void isCustomerExists(Integer id) throws BadRequestException, SQLException, ClassNotFoundException {
		Customer c = this.getCustomer(id);
		if (Boolean.FALSE.equals((c != null))) {
			throw new NotFoundException("Customer with id '" + id + "' not found.");
		}

		// should be updated
	}

	@Override
  public void createCustomer(String name, String password, String email) throws SQLException, ClassNotFoundException {

		Customer c = customerRepository.findOne(email);
		if (Boolean.FALSE.equals((c != null))) {
			throw new BadRequestException("Bad request. Email already taken.");
		}

    customerRepository.save(name, email, password);
  }

//  @Cacheable(key = "#id", value = "customer")
	@Override
  public Customer getCustomer(Integer id) throws SQLException, ClassNotFoundException {
    return customerRepository.findOne(id);
  }

	@Override
  public Customer getCustomer(String email) throws SQLException, ClassNotFoundException {
    return customerRepository.findOne(email);
  }

	@Override
  public List<Customer> getAll(Integer skip, Integer limit) throws SQLException, ClassNotFoundException {
    return customerRepository.findAll(skip, limit);
  }

	@Override
  public void updateProfile(Customer c) throws SQLException, ClassNotFoundException {
		this.isCustomerExists(c.email());
    customerRepository.update(c);
  }

	@Override
  public Integer changePassword(String email, String pwd) throws ServerErrorException, SQLException, ClassNotFoundException {
		this.isCustomerExists(email);
		Customer c = this.getCustomer(email);
//
//    if (c.isPresent()) {
//      Customer updated = new Customer(null, c.get().name(), email, pwd, c.get().createdAt(), new Date(System.),
//          c.get().isBanned(), c.get().role());
//      customerRepository.update(updated, c.get().id());
//      return c.get().id();
//    }
//    throw new ServerErrorException();
		return c.id();
  }

	@Override
  public void deleteCustomer(Integer idToDelete, Integer adminId) throws SQLException, ClassNotFoundException {
    customerRepository.delete(idToDelete, adminId);
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

		customerRepository.update2faStatus(dto);
		emailService.sendTwoStepCode(dto.email());
		actionService.saveCustomerAction(a);
	}


	@Override
	public Boolean getTwoStepStatus(String email) throws SQLException, ClassNotFoundException {
		return customerRepository.getTwoStepStatus(email);
	}
}

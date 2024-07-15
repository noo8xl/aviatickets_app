package aviatickets.app.customer;

import java.util.List;
import java.util.Optional;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import aviatickets.app.customer.dto.ChangePwdDto;
import aviatickets.app.customer.entity.Customer;
import aviatickets.app.exception.BadRequestException;
import aviatickets.app.exception.NotFoundException;
import aviatickets.app.exception.ServerErrorException;
import jakarta.validation.Valid;

@RestController
@RequestMapping("/api/customer")
public class CustomerController {

  private final CustomerService customerService;

  public CustomerController(CustomerService customerService) {
    this.customerService = customerService;
  }

  @ResponseStatus(HttpStatus.OK)
  @GetMapping("/")
  List<Customer> findAll() {
    try {
      List<Customer> customers = customerService.getAll();
      if (customers.isEmpty()) {
        throw new NotFoundException("Empty set.");
      }
      return customers;
    } catch (Exception e) {
      throw new ServerErrorException("Get user list was failed with " + e.getMessage());
    }
  }

  @ResponseStatus(HttpStatus.OK)
  @GetMapping("/get/{email}/")
  Customer findById(@PathVariable String email) {
    try {
      Optional<Customer> customer = customerService.getCustomer(email);
      if (customer.isEmpty()) {
        throw new NotFoundException("User not found.");
      }
      return customer.get();
    } catch (Exception e) {
      throw new ServerErrorException("Get user was failed with " + e.getMessage());
    }
  }

  @ResponseStatus(HttpStatus.OK)
  @GetMapping("/get/{id}/")
  Customer findById(@PathVariable Integer id) {

    try {
      Optional<Customer> customer = customerService.getCustomer(id);
      if (customer.isEmpty()) {
        throw new NotFoundException("User not found.");
      }
      return customer.get();
    } catch (RuntimeException e) {
      throw new ServerErrorException("Get user was failed with " + e.getMessage());
    }
  }

  @ResponseStatus(HttpStatus.CREATED)
  @PostMapping("/create/")
  void create(@Valid @RequestBody Customer customer) {
    try {
      Boolean isExist = customerService.isCustomerExists(customer.email());
      if (Boolean.TRUE.equals(isExist))
        throw new BadRequestException("Bad request. Email is already taken.");
      customerService.createCustomer(customer.name(), customer.password(), customer.email());
    } catch (Exception e) {
      throw new ServerErrorException("Create user was failed with " + e.getMessage());
    }
  }

  @ResponseStatus(HttpStatus.NO_CONTENT)
  @PutMapping("/update/{id}/")
  void update(@Valid @RequestBody Customer customer, @PathVariable Integer id) {
    try {
      Boolean isExist = customerService.isCustomerExists(customer.email());
      if (Boolean.FALSE.equals(isExist))
        throw new BadRequestException("Bad request. User not found.");
      customerService.updateProfile(id, customer);
    } catch (Exception e) {
      throw new ServerErrorException("Update user was failed with " + e.getMessage());
    }
  }

  @ResponseStatus(HttpStatus.ACCEPTED)
  @PatchMapping("/change-password/")
  void changePassword(@RequestBody ChangePwdDto dto) {
    try {
      Boolean isExist = customerService.isCustomerExists(dto.email());
      if (Boolean.FALSE.equals(isExist))
        throw new BadRequestException("Bad request. User not found.");
      customerService.changePassword(dto.email(), dto.pwd());
    } catch (Exception e) {
      throw new ServerErrorException("Change user password was failed with " + e.getMessage());
    }
  }

  @ResponseStatus(HttpStatus.NO_CONTENT)
  @DeleteMapping("/delete/{idToDelete}/{customerId}")
  void delete(@PathVariable Integer idToDelete, @PathVariable Integer customerId) {
    try {
      customerService.deleteCustomer(idToDelete, customerId);
    } catch (Exception e) {
      throw new ServerErrorException("catch an error at delete user " + e.getCause());
    }
  }

}

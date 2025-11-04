import { 
  Customer, 
  CreateCustomerRequest, 
  UpdateCustomerRequest 
} from '@/types/cardDemo';

const API_BASE_URL = '/api';

class CustomerService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getCustomers(): Promise<Customer[]> {
    const response = await fetch(`${API_BASE_URL}/customers`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch customers');
    }
    return response.json();
  }

  async getCustomerById(customerId: string): Promise<Customer> {
    const response = await fetch(`${API_BASE_URL}/customers/${customerId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch customer');
    }
    return response.json();
  }

  async getCustomersByLastName(lastName: string): Promise<Customer[]> {
    const response = await fetch(`${API_BASE_URL}/customers/lastname/${lastName}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch customers by last name');
    }
    return response.json();
  }

  async createCustomer(data: CreateCustomerRequest): Promise<Customer> {
    const response = await fetch(`${API_BASE_URL}/customers`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      throw new Error('Failed to create customer');
    }
    return response.json();
  }

  async updateCustomer(customerId: string, data: UpdateCustomerRequest): Promise<Customer> {
    const response = await fetch(`${API_BASE_URL}/customers/${customerId}`, {
      method: 'PUT',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      throw new Error('Failed to update customer');
    }
    return response.json();
  }

  async deleteCustomer(customerId: string): Promise<void> {
    const response = await fetch(`${API_BASE_URL}/customers/${customerId}`, {
      method: 'DELETE',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to delete customer');
    }
  }
}

export const customerService = new CustomerService();

import { Customer, UpdateCustomerRequest } from '@/types/account';

const API_BASE_URL = '/api';

class CustomerService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getCustomerById(id: string): Promise<Customer> {
    const response = await fetch(`${API_BASE_URL}/customers/${id}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      if (response.status === 404) {
        throw new Error('Customer not found');
      }
      throw new Error('Failed to fetch customer');
    }
    return response.json();
  }

  async updateCustomer(id: string, data: UpdateCustomerRequest): Promise<Customer> {
    const response = await fetch(`${API_BASE_URL}/customers/${id}`, {
      method: 'PUT',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      if (response.status === 404) {
        throw new Error('Customer not found');
      }
      if (response.status === 409) {
        throw new Error('Concurrent update detected. Please refresh and try again.');
      }
      throw new Error('Failed to update customer');
    }
    return response.json();
  }

  formatSSN(ssn: number): string {
    const ssnStr = ssn.toString().padStart(9, '0');
    return `${ssnStr.slice(0, 3)}-${ssnStr.slice(3, 5)}-${ssnStr.slice(5)}`;
  }

  maskSSN(ssn: number): string {
    const ssnStr = ssn.toString().padStart(9, '0');
    return `XXX-XX-${ssnStr.slice(5)}`;
  }
}

export const customerService = new CustomerService();

import { AdminMenuOption, CreateAdminMenuOptionRequest, UpdateAdminMenuOptionRequest } from '@/types/admin-menu-option';

const API_BASE_URL = '/api';

class AdminMenuOptionService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getAdminMenuOptions(page: number = 0, size: number = 20, sort?: string): Promise<any> {
    const queryParams = new URLSearchParams({
      page: page.toString(),
      size: size.toString(),
      ...(sort && { sort }),
    });
    
    const response = await fetch(`${API_BASE_URL}/admin-menu-options?${queryParams}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch admin menu options');
    }
    return response.json();
  }

  async getAdminMenuOptionById(id: number): Promise<AdminMenuOption> {
    const response = await fetch(`${API_BASE_URL}/admin-menu-options/${id}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch admin menu option');
    }
    return response.json();
  }

  async getActiveAdminMenuOptions(): Promise<AdminMenuOption[]> {
    const response = await fetch(`${API_BASE_URL}/admin-menu-options/active`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch active admin menu options');
    }
    return response.json();
  }

  async getInactiveAdminMenuOptions(): Promise<AdminMenuOption[]> {
    const response = await fetch(`${API_BASE_URL}/admin-menu-options/inactive`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch inactive admin menu options');
    }
    return response.json();
  }

  async getAdminMenuOptionByOptionNumber(optionNumber: number): Promise<AdminMenuOption> {
    const response = await fetch(`${API_BASE_URL}/admin-menu-options/option-number/${optionNumber}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch admin menu option');
    }
    return response.json();
  }

  async getAdminMenuOptionsByProgramName(programName: string): Promise<AdminMenuOption[]> {
    const response = await fetch(`${API_BASE_URL}/admin-menu-options/program/${programName}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch admin menu options by program');
    }
    return response.json();
  }

  async getAdminMenuOptionsByAdminUser(adminUserId: string): Promise<AdminMenuOption[]> {
    const response = await fetch(`${API_BASE_URL}/admin-menu-options/user/${adminUserId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch admin menu options by user');
    }
    return response.json();
  }

  async getActiveAdminMenuOptionsByAdminUser(adminUserId: string): Promise<AdminMenuOption[]> {
    const response = await fetch(`${API_BASE_URL}/admin-menu-options/user/${adminUserId}/active`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch active admin menu options by user');
    }
    return response.json();
  }

  async createAdminMenuOption(data: CreateAdminMenuOptionRequest): Promise<AdminMenuOption> {
    const response = await fetch(`${API_BASE_URL}/admin-menu-options`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      throw new Error('Failed to create admin menu option');
    }
    return response.json();
  }

  async updateAdminMenuOption(id: number, data: UpdateAdminMenuOptionRequest): Promise<AdminMenuOption> {
    const response = await fetch(`${API_BASE_URL}/admin-menu-options/${id}`, {
      method: 'PUT',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      throw new Error('Failed to update admin menu option');
    }
    return response.json();
  }

  async deleteAdminMenuOption(id: number): Promise<void> {
    const response = await fetch(`${API_BASE_URL}/admin-menu-options/${id}`, {
      method: 'DELETE',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to delete admin menu option');
    }
  }

  async activateAdminMenuOption(id: number): Promise<AdminMenuOption> {
    const response = await fetch(`${API_BASE_URL}/admin-menu-options/${id}/activate`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to activate admin menu option');
    }
    return response.json();
  }

  async deactivateAdminMenuOption(id: number): Promise<AdminMenuOption> {
    const response = await fetch(`${API_BASE_URL}/admin-menu-options/${id}/deactivate`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to deactivate admin menu option');
    }
    return response.json();
  }
}

export const adminMenuOptionService = new AdminMenuOptionService();

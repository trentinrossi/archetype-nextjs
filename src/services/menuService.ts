import { MenuOption, AdminMenuOption } from '@/types/menu-option';

const API_BASE_URL = '/api';

class MenuService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getActiveMenuOptions(): Promise<MenuOption[]> {
    const response = await fetch(`${API_BASE_URL}/menu-options/active`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch active menu options');
    }
    return response.json();
  }

  async getMenuOptionsForUserType(userType: string): Promise<MenuOption[]> {
    const response = await fetch(`${API_BASE_URL}/menu-options/for-user-type/${userType}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch menu options for user type');
    }
    return response.json();
  }

  async getMenuOptionByNumber(optionNumber: number): Promise<MenuOption> {
    const response = await fetch(`${API_BASE_URL}/menu-options/by-number/${optionNumber}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch menu option');
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

  async getAdminMenuOptionByNumber(optionNumber: number): Promise<AdminMenuOption> {
    const response = await fetch(`${API_BASE_URL}/admin-menu-options/by-number/${optionNumber}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch admin menu option');
    }
    return response.json();
  }
}

export const menuService = new MenuService();

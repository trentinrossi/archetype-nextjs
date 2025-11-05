import { Widget, CreateWidgetRequest, UpdateWidgetRequest, ApiError } from '@/types/widget';

const API_BASE_URL = '/api';

class WidgetService {
  private getAuthHeaders(): Record<string, string> {
    const token = typeof window !== 'undefined' ? localStorage.getItem('access_token') : null;
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  private async handleResponse<T>(response: Response): Promise<T> {
    if (!response.ok) {
      const errorData: ApiError = await response.json().catch(() => ({
        error: 'An unexpected error occurred',
        status: response.status,
      }));
      throw new Error(errorData.error || `HTTP error! status: ${response.status}`);
    }
    return response.json();
  }

  async getWidgets(): Promise<Widget[]> {
    try {
      const response = await fetch(`${API_BASE_URL}/widgets`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<Widget[]>(response);
    } catch (error) {
      console.error('Error fetching widgets:', error);
      throw error;
    }
  }

  async getWidgetById(id: string): Promise<Widget> {
    try {
      const response = await fetch(`${API_BASE_URL}/widgets/${id}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<Widget>(response);
    } catch (error) {
      console.error(`Error fetching widget ${id}:`, error);
      throw error;
    }
  }

  async getWidgetsByCategory(category: string): Promise<Widget[]> {
    try {
      const response = await fetch(`${API_BASE_URL}/widgets/category/${category}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<Widget[]>(response);
    } catch (error) {
      console.error(`Error fetching widgets by category ${category}:`, error);
      throw error;
    }
  }

  async createWidget(data: CreateWidgetRequest): Promise<Widget> {
    try {
      const response = await fetch(`${API_BASE_URL}/widgets`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(data),
      });
      return this.handleResponse<Widget>(response);
    } catch (error) {
      console.error('Error creating widget:', error);
      throw error;
    }
  }

  async updateWidget(id: string, data: UpdateWidgetRequest): Promise<Widget> {
    try {
      const response = await fetch(`${API_BASE_URL}/widgets/${id}`, {
        method: 'PUT',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(data),
      });
      return this.handleResponse<Widget>(response);
    } catch (error) {
      console.error(`Error updating widget ${id}:`, error);
      throw error;
    }
  }

  async deleteWidget(id: string): Promise<void> {
    try {
      const response = await fetch(`${API_BASE_URL}/widgets/${id}`, {
        method: 'DELETE',
        headers: this.getAuthHeaders(),
      });
      if (!response.ok) {
        const errorData: ApiError = await response.json().catch(() => ({
          error: 'An unexpected error occurred',
          status: response.status,
        }));
        throw new Error(errorData.error || `HTTP error! status: ${response.status}`);
      }
    } catch (error) {
      console.error(`Error deleting widget ${id}:`, error);
      throw error;
    }
  }
}

export const widgetService = new WidgetService();
export default WidgetService;

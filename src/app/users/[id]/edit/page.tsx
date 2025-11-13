'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { userService } from '@/services/userService';
import { User, UpdateUserRequest } from '@/types/user';
import { Input, Select, Button } from '@/components/ui';

export default function EditUserPage() {
  const params = useParams();
  const router = useRouter();
  const [originalUser, setOriginalUser] = useState<User | null>(null);
  const [formData, setFormData] = useState<UpdateUserRequest>({});
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [errorMessage, setErrorMessage] = useState('');
  const [successMessage, setSuccessMessage] = useState('');
  const [currentDate, setCurrentDate] = useState('');
  const [currentTime, setCurrentTime] = useState('');

  useEffect(() => {
    const user = userService.getCurrentUser();
    if (!user || user.userType !== 'A') {
      router.push('/login');
      return;
    }

    if (params.id) {
      fetchUser(params.id as string);
    }

    updateDateTime();
    const interval = setInterval(updateDateTime, 1000);
    return () => clearInterval(interval);
  }, [params.id, router]);

  const updateDateTime = () => {
    const now = new Date();
    setCurrentDate(now.toLocaleDateString('en-US', { 
      month: '2-digit', 
      day: '2-digit', 
      year: 'numeric' 
    }));
    setCurrentTime(now.toLocaleTimeString('en-US', { 
      hour: '2-digit', 
      minute: '2-digit', 
      second: '2-digit',
      hour12: false 
    }));
  };

  const fetchUser = async (id: string) => {
    try {
      const data = await userService.getUserById(parseInt(id));
      setOriginalUser(data);
      setFormData({
        firstName: data.firstName,
        lastName: data.lastName,
        userType: data.userType,
        authenticated: data.authenticated,
        password: '',
      });
      setSuccessMessage('User data retrieved successfully. You can now modify the fields.');
    } catch (err) {
      console.error('Failed to load user:', err);
      setErrorMessage('Failed to load user');
    } finally {
      setLoading(false);
    }
  };

  const validateForm = (): boolean => {
    setErrorMessage('');

    if (!formData.firstName?.trim()) {
      setErrorMessage('First Name can NOT be empty...');
      return false;
    }

    if (!formData.lastName?.trim()) {
      setErrorMessage('Last Name can NOT be empty...');
      return false;
    }

    if (!formData.userType) {
      setErrorMessage('User Type can NOT be empty...');
      return false;
    }

    return true;
  };

  const hasChanges = (): boolean => {
    if (!originalUser) return false;
    
    return (
      formData.firstName !== originalUser.firstName ||
      formData.lastName !== originalUser.lastName ||
      formData.userType !== originalUser.userType ||
      formData.authenticated !== originalUser.authenticated ||
      (formData.password && formData.password.trim() !== '')
    );
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!validateForm()) {
      return;
    }

    if (!hasChanges()) {
      setErrorMessage('No modifications detected. Please make changes before saving.');
      return;
    }

    try {
      setSaving(true);
      setErrorMessage('');
      setSuccessMessage('');
      
      const updateData: UpdateUserRequest = {
        firstName: formData.firstName,
        lastName: formData.lastName,
        userType: formData.userType,
        authenticated: formData.authenticated,
      };

      if (formData.password && formData.password.trim() !== '') {
        updateData.password = formData.password;
      }

      const updatedUser = await userService.updateUser(parseInt(params.id as string), updateData);
      setSuccessMessage(`User ${updatedUser.userId} updated successfully!`);
      setOriginalUser(updatedUser);
      
      setTimeout(() => {
        router.push(`/users/${params.id}`);
      }, 2000);
    } catch (err: any) {
      setErrorMessage(err.message || 'Unable to Update User...');
    } finally {
      setSaving(false);
    }
  };

  const handleClear = () => {
    if (originalUser) {
      setFormData({
        firstName: originalUser.firstName,
        lastName: originalUser.lastName,
        userType: originalUser.userType,
        authenticated: originalUser.authenticated,
        password: '',
      });
      setErrorMessage('');
      setSuccessMessage('');
    }
  };

  if (loading) {
    return (
      <div className="min-h-screen bg-gray-50 flex items-center justify-center">
        <div className="text-lg">Loading...</div>
      </div>
    );
  }

  if (!originalUser) {
    return (
      <div className="min-h-screen bg-gray-50 flex items-center justify-center">
        <div className="text-lg">User not found</div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-50 flex flex-col">
      <div className="bg-blue-900 text-white p-4">
        <div className="max-w-7xl mx-auto flex justify-between items-center">
          <div>
            <h1 className="text-2xl font-bold">AWS Mainframe Modernization</h1>
            <h2 className="text-lg">CardDemo - Update User</h2>
          </div>
          <div className="text-right">
            <div className="text-sm">Date: {currentDate}</div>
            <div className="text-sm">Time: {currentTime}</div>
          </div>
        </div>
      </div>

      <div className="flex-1 p-6">
        <div className="max-w-2xl mx-auto">
          <div className="bg-white shadow-lg rounded-lg p-8">
            <div className="mb-6">
              <div className="text-sm text-gray-600 mb-4">
                <span>Transaction: COUSR</span>
                <span className="ml-4">Program: COUSR02C</span>
              </div>
              <h3 className="text-xl font-bold text-gray-800">Update User: {originalUser.userId}</h3>
            </div>

            {errorMessage && (
              <div className="mb-4 p-3 bg-red-50 border border-red-200 rounded text-red-700 text-sm">
                {errorMessage}
              </div>
            )}

            {successMessage && (
              <div className="mb-4 p-3 bg-green-50 border border-green-200 rounded text-green-700 text-sm">
                {successMessage}
              </div>
            )}

            <form onSubmit={handleSubmit} className="space-y-4">
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-1">User ID</label>
                <div className="px-3 py-2 bg-gray-100 border border-gray-300 rounded-md text-gray-700">
                  {originalUser.userId}
                </div>
              </div>

              <Input
                label="First Name"
                value={formData.firstName || ''}
                onChange={(e) => setFormData({ ...formData, firstName: e.target.value })}
                required
              />
              
              <Input
                label="Last Name"
                value={formData.lastName || ''}
                onChange={(e) => setFormData({ ...formData, lastName: e.target.value })}
                required
              />
              
              <Input
                label="Password (leave blank to keep current)"
                type="password"
                value={formData.password || ''}
                onChange={(e) => setFormData({ ...formData, password: e.target.value })}
                placeholder="Enter new password or leave blank"
              />
              
              <Select
                label="User Type"
                value={formData.userType || 'R'}
                onChange={(e) => setFormData({ ...formData, userType: e.target.value })}
                options={[
                  { value: 'R', label: 'Regular User' },
                  { value: 'A', label: 'Administrator' },
                ]}
                required
              />

              <div className="flex items-center">
                <input
                  type="checkbox"
                  id="authenticated"
                  checked={formData.authenticated || false}
                  onChange={(e) => setFormData({ ...formData, authenticated: e.target.checked })}
                  className="h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300 rounded"
                />
                <label htmlFor="authenticated" className="ml-2 block text-sm text-gray-700">
                  Authenticated
                </label>
              </div>
              
              <div className="flex gap-2 pt-4">
                <Button type="submit" disabled={saving}>
                  {saving ? 'Saving...' : 'Save Changes'}
                </Button>
                <Button
                  type="button"
                  variant="secondary"
                  onClick={handleClear}
                >
                  Reset
                </Button>
                <Button
                  type="button"
                  variant="secondary"
                  onClick={() => router.push(`/users/${params.id}`)}
                >
                  Cancel
                </Button>
              </div>
            </form>
          </div>
        </div>
      </div>
    </div>
  );
}

'use client';

import React, { useState, useEffect } from 'react';
import { useRouter, useParams } from 'next/navigation';
import { useAuth } from '@/contexts/AuthContext';
import { userService } from '@/services/userService';
import { Button, Input } from '@/components/ui';

export default function UserEditPage() {
  const router = useRouter();
  const params = useParams();
  const userId = params.userId as string;
  const { isAuthenticated, isAdmin } = useAuth();
  const [formData, setFormData] = useState({
    firstName: '',
    lastName: '',
    password: '',
    userType: 'R' as 'A' | 'R',
  });
  const [errors, setErrors] = useState<Record<string, string>>({});
  const [successMessage, setSuccessMessage] = useState('');
  const [loading, setLoading] = useState(false);
  const [loadingUser, setLoadingUser] = useState(true);
  const [currentDateTime, setCurrentDateTime] = useState('');

  useEffect(() => {
    if (!isAuthenticated || !isAdmin()) {
      router.push('/login');
      return;
    }
  }, [isAuthenticated, isAdmin, router]);

  useEffect(() => {
    const updateDateTime = () => {
      const now = new Date();
      const date = now.toLocaleDateString('en-US', { month: '2-digit', day: '2-digit', year: '2-digit' });
      const time = now.toLocaleTimeString('en-US', { hour: '2-digit', minute: '2-digit', second: '2-digit', hour12: false });
      setCurrentDateTime(`${date} ${time}`);
    };
    updateDateTime();
    const interval = setInterval(updateDateTime, 1000);
    return () => clearInterval(interval);
  }, []);

  useEffect(() => {
    if (userId) {
      fetchUser();
    }
  }, [userId]);

  const fetchUser = async () => {
    try {
      setLoadingUser(true);
      const user = await userService.getUserById(userId);
      setFormData({
        firstName: user.firstName,
        lastName: user.lastName,
        password: user.password || '',
        userType: user.userType,
      });
      setSuccessMessage('Press PF5 key to save your updates ...');
    } catch (err) {
      setErrors({ general: err instanceof Error ? err.message : 'User ID NOT found...' });
    } finally {
      setLoadingUser(false);
    }
  };

  const validateForm = () => {
    const newErrors: Record<string, string> = {};

    if (!formData.firstName.trim()) {
      newErrors.firstName = 'First Name can NOT be empty...';
    }
    if (!formData.lastName.trim()) {
      newErrors.lastName = 'Last Name can NOT be empty...';
    }
    if (!formData.password.trim()) {
      newErrors.password = 'Password can NOT be empty...';
    }
    if (!formData.userType) {
      newErrors.userType = 'User Type can NOT be empty...';
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!validateForm()) {
      return;
    }

    try {
      setLoading(true);
      setSuccessMessage('');
      setErrors({});

      const response = await userService.updateUser(userId, formData);
      setSuccessMessage(response.message);
    } catch (err) {
      const errorMessage = err instanceof Error ? err.message : 'Failed to update user';
      setErrors({ general: errorMessage });
    } finally {
      setLoading(false);
    }
  };

  if (loadingUser) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-gray-50">
        <div className="text-center">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto"></div>
          <p className="mt-4 text-gray-600">Loading user...</p>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-50">
      <div className="bg-gradient-to-r from-blue-600 to-indigo-600 text-white shadow-lg">
        <div className="max-w-7xl mx-auto px-4 py-4">
          <div className="flex justify-between items-center">
            <div>
              <h1 className="text-2xl font-bold">CU02 - COUSR02C</h1>
              <p className="text-blue-100 text-sm">User Update (Security)</p>
            </div>
            <p className="text-sm">{currentDateTime}</p>
          </div>
        </div>
      </div>

      <div className="max-w-2xl mx-auto px-4 py-6">
        <div className="bg-white rounded-lg shadow-lg p-6">
          {successMessage && (
            <div className="mb-6 p-4 bg-green-50 border border-green-200 rounded-lg text-green-800">
              {successMessage}
            </div>
          )}

          {errors.general && (
            <div className="mb-6 p-4 bg-red-50 border border-red-200 rounded-lg text-red-800">
              {errors.general}
            </div>
          )}

          <div className="mb-6 p-4 bg-blue-50 border border-blue-200 rounded-lg">
            <p className="text-sm font-medium text-blue-900">User ID: {userId}</p>
          </div>

          <form onSubmit={handleSubmit} className="space-y-6">
            <div>
              <Input
                label="First Name"
                type="text"
                value={formData.firstName}
                onChange={(e) => setFormData({ ...formData, firstName: e.target.value })}
                error={errors.firstName}
                maxLength={20}
                required
              />
            </div>

            <div>
              <Input
                label="Last Name"
                type="text"
                value={formData.lastName}
                onChange={(e) => setFormData({ ...formData, lastName: e.target.value })}
                error={errors.lastName}
                maxLength={20}
                required
              />
            </div>

            <div>
              <Input
                label="Password"
                type="password"
                value={formData.password}
                onChange={(e) => setFormData({ ...formData, password: e.target.value.toUpperCase() })}
                error={errors.password}
                maxLength={8}
                required
              />
            </div>

            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                User Type <span className="text-red-500">*</span>
              </label>
              <select
                value={formData.userType}
                onChange={(e) => setFormData({ ...formData, userType: e.target.value as 'A' | 'R' })}
                className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                required
              >
                <option value="R">Regular User</option>
                <option value="A">Administrator</option>
              </select>
              {errors.userType && (
                <p className="mt-1 text-sm text-red-600">{errors.userType}</p>
              )}
            </div>

            <div className="flex gap-4 pt-4">
              <Button type="submit" disabled={loading} className="flex-1">
                {loading ? 'Updating User...' : 'Update User (PF5)'}
              </Button>
            </div>
          </form>

          <div className="mt-6 pt-6 border-t border-gray-200 flex gap-4">
            <Button variant="secondary" onClick={() => router.push('/admin/users')}>
              PF3 - Return to List
            </Button>
            <Button variant="secondary" onClick={() => router.push('/admin')}>
              PF12 - Return to Menu
            </Button>
          </div>
        </div>
      </div>
    </div>
  );
}

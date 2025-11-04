'use client';

import React, { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { useAuth } from '@/contexts/AuthContext';
import { userService } from '@/services/userService';
import { Button, Input } from '@/components/ui';

export default function UserAddPage() {
  const router = useRouter();
  const { isAuthenticated, isAdmin } = useAuth();
  const [formData, setFormData] = useState({
    userId: '',
    firstName: '',
    lastName: '',
    password: '',
    userType: 'R' as 'A' | 'R',
  });
  const [errors, setErrors] = useState<Record<string, string>>({});
  const [successMessage, setSuccessMessage] = useState('');
  const [loading, setLoading] = useState(false);
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

  const validateForm = () => {
    const newErrors: Record<string, string> = {};

    if (!formData.userId.trim()) {
      newErrors.userId = 'User ID can NOT be empty...';
    }
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

      const response = await userService.createUser(formData);
      setSuccessMessage(response.message);
      
      // Clear form
      setFormData({
        userId: '',
        firstName: '',
        lastName: '',
        password: '',
        userType: 'R',
      });
    } catch (err) {
      const errorMessage = err instanceof Error ? err.message : 'Failed to create user';
      setErrors({ general: errorMessage });
    } finally {
      setLoading(false);
    }
  };

  const handleClear = () => {
    setFormData({
      userId: '',
      firstName: '',
      lastName: '',
      password: '',
      userType: 'R',
    });
    setErrors({});
    setSuccessMessage('');
  };

  return (
    <div className="min-h-screen bg-gray-50">
      <div className="bg-gradient-to-r from-blue-600 to-indigo-600 text-white shadow-lg">
        <div className="max-w-7xl mx-auto px-4 py-4">
          <div className="flex justify-between items-center">
            <div>
              <h1 className="text-2xl font-bold">CU01 - COUSR01C</h1>
              <p className="text-blue-100 text-sm">User Add (Security)</p>
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

          <form onSubmit={handleSubmit} className="space-y-6">
            <div>
              <Input
                label="User ID"
                type="text"
                value={formData.userId}
                onChange={(e) => setFormData({ ...formData, userId: e.target.value.toUpperCase() })}
                error={errors.userId}
                maxLength={8}
                required
              />
            </div>

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
                {loading ? 'Adding User...' : 'Add User (Enter)'}
              </Button>
              <Button type="button" variant="secondary" onClick={handleClear} className="flex-1">
                Clear (PF4)
              </Button>
            </div>
          </form>

          <div className="mt-6 pt-6 border-t border-gray-200">
            <Button variant="secondary" onClick={() => router.push('/admin')}>
              PF3 - Return to Menu
            </Button>
          </div>
        </div>
      </div>
    </div>
  );
}

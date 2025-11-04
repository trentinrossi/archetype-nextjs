'use client';

import React, { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { useAuth } from '@/contexts/AuthContext';
import { userService } from '@/services/userService';
import { Button, Input } from '@/components/ui';
import { User } from '@/types/user';

export default function ProfilePage() {
  const router = useRouter();
  const { user: authUser, isAuthenticated } = useAuth();
  const [user, setUser] = useState<User | null>(null);
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
    if (!isAuthenticated || !authUser) {
      router.push('/login');
      return;
    }
  }, [isAuthenticated, authUser, router]);

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
    if (authUser) {
      fetchUser();
    }
  }, [authUser]);

  const fetchUser = async () => {
    if (!authUser) return;
    
    try {
      setLoadingUser(true);
      const userData = await userService.getUserById(authUser.userId);
      setUser(userData);
      setFormData({
        firstName: userData.firstName,
        lastName: userData.lastName,
        password: userData.password || '',
        userType: userData.userType,
      });
    } catch (err) {
      setErrors({ general: err instanceof Error ? err.message : 'Failed to load profile' });
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

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!validateForm() || !authUser) {
      return;
    }

    try {
      setLoading(true);
      setSuccessMessage('');
      setErrors({});

      const response = await userService.updateUser(authUser.userId, formData);
      setSuccessMessage(response.message);
    } catch (err) {
      const errorMessage = err instanceof Error ? err.message : 'Failed to update profile';
      setErrors({ general: errorMessage });
    } finally {
      setLoading(false);
    }
  };

  if (loadingUser) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-gray-50">
        <div className="text-center">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-green-600 mx-auto"></div>
          <p className="mt-4 text-gray-600">Loading profile...</p>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-50">
      <div className="bg-gradient-to-r from-green-600 to-teal-600 text-white shadow-lg">
        <div className="max-w-7xl mx-auto px-4 py-4">
          <div className="flex justify-between items-center">
            <div>
              <h1 className="text-2xl font-bold">My Profile</h1>
              <p className="text-green-100 text-sm">View and update your profile information</p>
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

          {user && (
            <>
              <div className="mb-6 p-4 bg-blue-50 border border-blue-200 rounded-lg">
                <div className="flex justify-between items-center">
                  <div>
                    <p className="text-sm font-medium text-blue-900">User ID: {user.userId}</p>
                    <p className="text-sm text-blue-700 mt-1">
                      Account Type: {user.userType === 'A' ? 'Administrator' : 'Regular User'}
                    </p>
                  </div>
                  <span className={`px-3 py-1 rounded-full text-xs font-semibold ${user.userType === 'A' ? 'bg-purple-100 text-purple-800' : 'bg-green-100 text-green-800'}`}>
                    {user.userType === 'A' ? 'Admin' : 'Regular'}
                  </span>
                </div>
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

                <div className="pt-4">
                  <Button type="submit" disabled={loading} className="w-full">
                    {loading ? 'Updating Profile...' : 'Update Profile'}
                  </Button>
                </div>
              </form>
            </>
          )}

          <div className="mt-6 pt-6 border-t border-gray-200">
            <Button variant="secondary" onClick={() => router.push('/menu')}>
              Return to Menu
            </Button>
          </div>
        </div>
      </div>
    </div>
  );
}

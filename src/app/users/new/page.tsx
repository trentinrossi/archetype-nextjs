'use client';

import React, { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { userService } from '@/services/userService';
import { CreateUserRequest } from '@/types/user';
import { Input, Select, Button } from '@/components/ui';

export default function CreateUserPage() {
  const router = useRouter();
  const [formData, setFormData] = useState<CreateUserRequest>({
    userType: 'R',
    authenticated: false,
    userId: '',
    password: '',
    firstName: '',
    lastName: '',
  });
  const [loading, setLoading] = useState(false);
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

    updateDateTime();
    const interval = setInterval(updateDateTime, 1000);
    return () => clearInterval(interval);
  }, [router]);

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

  const validateForm = (): boolean => {
    setErrorMessage('');

    if (!formData.firstName.trim()) {
      setErrorMessage('First Name can NOT be empty...');
      return false;
    }

    if (!formData.lastName.trim()) {
      setErrorMessage('Last Name can NOT be empty...');
      return false;
    }

    if (!formData.userId.trim()) {
      setErrorMessage('User ID can NOT be empty...');
      return false;
    }

    if (formData.userId.length > 8) {
      setErrorMessage('User ID must be 8 characters or less');
      return false;
    }

    if (!formData.password.trim()) {
      setErrorMessage('Password can NOT be empty...');
      return false;
    }

    if (!formData.userType) {
      setErrorMessage('User Type can NOT be empty...');
      return false;
    }

    return true;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!validateForm()) {
      return;
    }

    try {
      setLoading(true);
      setErrorMessage('');
      setSuccessMessage('');
      
      const newUser = await userService.createUser(formData);
      setSuccessMessage(`User ${newUser.userId} added successfully!`);
      
      setTimeout(() => {
        router.push('/users');
      }, 2000);
    } catch (err: any) {
      if (err.message.includes('already exist')) {
        setErrorMessage('User ID already exist...');
      } else {
        setErrorMessage(err.message || 'Failed to create user');
      }
    } finally {
      setLoading(false);
    }
  };

  const handleClear = () => {
    setFormData({
      userType: 'R',
      authenticated: false,
      userId: '',
      password: '',
      firstName: '',
      lastName: '',
    });
    setErrorMessage('');
    setSuccessMessage('');
  };

  return (
    <div className="min-h-screen bg-gray-50 flex flex-col">
      <div className="bg-blue-900 text-white p-4">
        <div className="max-w-7xl mx-auto flex justify-between items-center">
          <div>
            <h1 className="text-2xl font-bold">AWS Mainframe Modernization</h1>
            <h2 className="text-lg">CardDemo - Add New User</h2>
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
                <span className="ml-4">Program: COUSR01C</span>
              </div>
              <h3 className="text-xl font-bold text-gray-800">Add New User</h3>
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
              <Input
                label="First Name"
                value={formData.firstName}
                onChange={(e) => setFormData({ ...formData, firstName: e.target.value })}
                required
                autoFocus
              />
              
              <Input
                label="Last Name"
                value={formData.lastName}
                onChange={(e) => setFormData({ ...formData, lastName: e.target.value })}
                required
              />
              
              <Input
                label="User ID"
                value={formData.userId}
                onChange={(e) => setFormData({ ...formData, userId: e.target.value.toUpperCase() })}
                maxLength={8}
                required
              />
              
              <Input
                label="Password"
                type="password"
                value={formData.password}
                onChange={(e) => setFormData({ ...formData, password: e.target.value })}
                required
              />
              
              <Select
                label="User Type"
                value={formData.userType}
                onChange={(e) => setFormData({ ...formData, userType: e.target.value })}
                options={[
                  { value: 'R', label: 'Regular User' },
                  { value: 'A', label: 'Administrator' },
                ]}
                required
              />
              
              <div className="flex gap-2 pt-4">
                <Button type="submit" disabled={loading}>
                  {loading ? 'Adding User...' : 'Add User'}
                </Button>
                <Button
                  type="button"
                  variant="secondary"
                  onClick={handleClear}
                >
                  Clear
                </Button>
                <Button
                  type="button"
                  variant="secondary"
                  onClick={() => router.push('/users')}
                >
                  Back to List
                </Button>
              </div>
            </form>
          </div>
        </div>
      </div>
    </div>
  );
}

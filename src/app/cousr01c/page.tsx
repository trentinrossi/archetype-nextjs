'use client';

import { useState, useEffect, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { Button, Input, Select } from '@/components/ui';
import { userSecurityService } from '@/services/userSecurityService';
import { 
  CreateUserSecurityRequest,
  UserSecurityDTO,
  ValidationResult,
  ApiError 
} from '@/types/userSecurity';

interface FormData {
  firstName: string;
  lastName: string;
  userId: string;
  password: string;
  userType: 'ADMIN' | 'GENERAL';
}

interface FormErrors {
  firstName?: string;
  lastName?: string;
  userId?: string;
  password?: string;
  userType?: string;
  general?: string;
}

export default function AddUserPage() {
  const router = useRouter();
  
  const [formData, setFormData] = useState<FormData>({
    firstName: '',
    lastName: '',
    userId: '',
    password: '',
    userType: 'GENERAL'
  });
  
  const [errors, setErrors] = useState<FormErrors>({});
  const [loading, setLoading] = useState(false);
  const [success, setSuccess] = useState<string | null>(null);
  const [currentDateTime, setCurrentDateTime] = useState('');

  // Update current date/time
  useEffect(() => {
    const updateDateTime = () => {
      const now = new Date();
      const formatted = now.toLocaleString('en-US', {
        year: 'numeric',
        month: '2-digit',
        day: '2-digit',
        hour: '2-digit',
        minute: '2-digit',
        second: '2-digit',
        hour12: false
      });
      setCurrentDateTime(formatted);
    };

    updateDateTime();
    const interval = setInterval(updateDateTime, 1000);
    return () => clearInterval(interval);
  }, []);

  const validateForm = (): boolean => {
    const newErrors: FormErrors = {};

    // First Name validation
    if (!formData.firstName.trim()) {
      newErrors.firstName = 'First Name is required';
    } else if (formData.firstName.length > 25) {
      newErrors.firstName = 'First Name cannot exceed 25 characters';
    }

    // Last Name validation
    if (!formData.lastName.trim()) {
      newErrors.lastName = 'Last Name is required';
    } else if (formData.lastName.length > 25) {
      newErrors.lastName = 'Last Name cannot exceed 25 characters';
    }

    // User ID validation
    if (!formData.userId.trim()) {
      newErrors.userId = 'User ID is required';
    } else if (formData.userId.length > 8) {
      newErrors.userId = 'User ID cannot exceed 8 characters';
    } else if (!/^[A-Z0-9]+$/.test(formData.userId)) {
      newErrors.userId = 'User ID must contain only uppercase letters and numbers';
    }

    // Password validation
    if (!formData.password.trim()) {
      newErrors.password = 'Password is required';
    } else if (formData.password.length < 4) {
      newErrors.password = 'Password must be at least 4 characters';
    } else if (formData.password.length > 8) {
      newErrors.password = 'Password cannot exceed 8 characters';
    }

    // User Type validation
    if (!formData.userType) {
      newErrors.userType = 'User Type is required';
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleInputChange = (field: keyof FormData, value: string) => {
    setFormData(prev => ({
      ...prev,
      [field]: field === 'userId' ? value.toUpperCase() : value
    }));
    
    // Clear field-specific error when user starts typing
    if (errors[field]) {
      setErrors(prev => ({
        ...prev,
        [field]: undefined
      }));
    }
    
    // Clear general error
    if (errors.general) {
      setErrors(prev => ({
        ...prev,
        general: undefined
      }));
    }
    
    // Clear success message
    if (success) {
      setSuccess(null);
    }
  };

  const handleSubmit = async () => {
    if (!validateForm()) {
      return;
    }

    setLoading(true);
    setErrors({});
    setSuccess(null);

    try {
      const createRequest: CreateUserSecurityRequest = {
        userId: formData.userId.trim(),
        password: formData.password,
        userType: formData.userType,
        programName: 'COUSR01C',
        transactionId: `ADD-${Date.now()}`,
        active: true
      };

      const newUser: UserSecurityDTO = await userSecurityService.createUser(createRequest);
      
      setSuccess(`User ${newUser.userId} has been successfully created.`);
      
      // Clear form after successful creation
      setFormData({
        firstName: '',
        lastName: '',
        userId: '',
        password: '',
        userType: 'GENERAL'
      });
      
    } catch (error) {
      if (error instanceof Error) {
        if (error.message.includes('duplicate') || error.message.includes('already exists')) {
          setErrors({ userId: 'User ID already exists. Please choose a different User ID.' });
        } else if (error.message.includes('validation')) {
          setErrors({ general: 'Validation failed. Please check all fields and try again.' });
        } else {
          setErrors({ general: error.message });
        }
      } else {
        setErrors({ general: 'An unexpected error occurred. Please try again.' });
      }
    } finally {
      setLoading(false);
    }
  };

  const handleSaveAndExit = async () => {
    if (!validateForm()) {
      return;
    }

    setLoading(true);
    setErrors({});

    try {
      const createRequest: CreateUserSecurityRequest = {
        userId: formData.userId.trim(),
        password: formData.password,
        userType: formData.userType,
        programName: 'COUSR01C',
        transactionId: `ADD-${Date.now()}`,
        active: true
      };

      await userSecurityService.createUser(createRequest);
      router.push('/coadm01c');
      
    } catch (error) {
      if (error instanceof Error) {
        if (error.message.includes('duplicate') || error.message.includes('already exists')) {
          setErrors({ userId: 'User ID already exists. Please choose a different User ID.' });
        } else if (error.message.includes('validation')) {
          setErrors({ general: 'Validation failed. Please check all fields and try again.' });
        } else {
          setErrors({ general: error.message });
        }
      } else {
        setErrors({ general: 'An unexpected error occurred. Please try again.' });
      }
      setLoading(false);
    }
  };

  const handleClearAll = () => {
    setFormData({
      firstName: '',
      lastName: '',
      userId: '',
      password: '',
      userType: 'GENERAL'
    });
    setErrors({});
    setSuccess(null);
  };

  const handleCancel = () => {
    router.push('/coadm01c');
  };

  const handleKeyPress = (e: React.KeyboardEvent) => {
    const key = e.key;
    
    if (key === 'F3') {
      e.preventDefault();
      handleSaveAndExit();
      return;
    }
    
    if (key === 'F4') {
      e.preventDefault();
      handleClearAll();
      return;
    }
    
    if (key === 'F12') {
      e.preventDefault();
      handleCancel();
      return;
    }
    
    if (key === 'Enter') {
      e.preventDefault();
      handleSubmit();
      return;
    }
  };

  const userTypeOptions = [
    { value: 'GENERAL', label: 'General User' },
    { value: 'ADMIN', label: 'Administrator' }
  ];

  return (
    <div className="min-h-screen bg-gray-50" onKeyDown={handleKeyPress}>
      {/* Header */}
      <div className="bg-blue-600 text-white p-4">
        <div className="flex justify-between items-center">
          <div>
            <h1 className="text-xl font-bold">COUSR01C - Add User</h1>
            <p className="text-sm opacity-90">CardDemo - User Security Administration</p>
          </div>
          <div className="text-right text-sm">
            <p>Program: COUSR01C</p>
            <p>Date/Time: {currentDateTime}</p>
          </div>
        </div>
      </div>

      {/* Main Content */}
      <div className="p-6">
        <div className="max-w-2xl mx-auto">
          {/* Success Message */}
          {success && (
            <div className="mb-6 p-4 bg-green-100 border border-green-400 text-green-700 rounded">
              <div className="flex items-center">
                <svg className="w-5 h-5 mr-2" fill="currentColor" viewBox="0 0 20 20">
                  <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z" clipRule="evenodd" />
                </svg>
                {success}
              </div>
            </div>
          )}

          {/* General Error Message */}
          {errors.general && (
            <div className="mb-6 p-4 bg-red-100 border border-red-400 text-red-700 rounded">
              <div className="flex items-center">
                <svg className="w-5 h-5 mr-2" fill="currentColor" viewBox="0 0 20 20">
                  <path fillRule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7 4a1 1 0 11-2 0 1 1 0 012 0zm-1-9a1 1 0 00-1 1v4a1 1 0 102 0V6a1 1 0 00-1-1z" clipRule="evenodd" />
                </svg>
                {errors.general}
              </div>
            </div>
          )}

          {/* Add User Form */}
          <div className="bg-white rounded-lg shadow-md p-6">
            <h2 className="text-lg font-semibold text-gray-900 mb-6">User Information</h2>
            
            <div className="space-y-6">
              {/* First Name */}
              <div>
                <label htmlFor="firstName" className="block text-sm font-medium text-gray-700 mb-2">
                  First Name <span className="text-red-500">*</span>
                </label>
                <Input
                  id="firstName"
                  type="text"
                  value={formData.firstName}
                  onChange={(e) => handleInputChange('firstName', e.target.value)}
                  maxLength={25}
                  placeholder="Enter first name"
                  disabled={loading}
                  className={`w-full ${errors.firstName ? 'border-red-500' : ''}`}
                />
                {errors.firstName && (
                  <p className="mt-1 text-sm text-red-600">{errors.firstName}</p>
                )}
              </div>

              {/* Last Name */}
              <div>
                <label htmlFor="lastName" className="block text-sm font-medium text-gray-700 mb-2">
                  Last Name <span className="text-red-500">*</span>
                </label>
                <Input
                  id="lastName"
                  type="text"
                  value={formData.lastName}
                  onChange={(e) => handleInputChange('lastName', e.target.value)}
                  maxLength={25}
                  placeholder="Enter last name"
                  disabled={loading}
                  className={`w-full ${errors.lastName ? 'border-red-500' : ''}`}
                />
                {errors.lastName && (
                  <p className="mt-1 text-sm text-red-600">{errors.lastName}</p>
                )}
              </div>

              {/* User ID */}
              <div>
                <label htmlFor="userId" className="block text-sm font-medium text-gray-700 mb-2">
                  User ID <span className="text-red-500">*</span>
                </label>
                <Input
                  id="userId"
                  type="text"
                  value={formData.userId}
                  onChange={(e) => handleInputChange('userId', e.target.value)}
                  maxLength={8}
                  placeholder="Enter user ID (max 8 chars)"
                  disabled={loading}
                  className={`w-full ${errors.userId ? 'border-red-500' : ''}`}
                />
                {errors.userId && (
                  <p className="mt-1 text-sm text-red-600">{errors.userId}</p>
                )}
                <p className="mt-1 text-xs text-gray-500">
                  Must be uppercase letters and numbers only, maximum 8 characters
                </p>
              </div>

              {/* Password */}
              <div>
                <label htmlFor="password" className="block text-sm font-medium text-gray-700 mb-2">
                  Password <span className="text-red-500">*</span>
                </label>
                <Input
                  id="password"
                  type="password"
                  value={formData.password}
                  onChange={(e) => handleInputChange('password', e.target.value)}
                  maxLength={8}
                  placeholder="Enter password (4-8 chars)"
                  disabled={loading}
                  className={`w-full ${errors.password ? 'border-red-500' : ''}`}
                />
                {errors.password && (
                  <p className="mt-1 text-sm text-red-600">{errors.password}</p>
                )}
                <p className="mt-1 text-xs text-gray-500">
                  Must be between 4 and 8 characters
                </p>
              </div>

              {/* User Type */}
              <div>
                <label htmlFor="userType" className="block text-sm font-medium text-gray-700 mb-2">
                  User Type <span className="text-red-500">*</span>
                </label>
                <Select
                  id="userType"
                  value={formData.userType}
                  onChange={(e) => handleInputChange('userType', e.target.value as 'ADMIN' | 'GENERAL')}
                  options={userTypeOptions}
                  disabled={loading}
                  className={`w-full ${errors.userType ? 'border-red-500' : ''}`}
                />
                {errors.userType && (
                  <p className="mt-1 text-sm text-red-600">{errors.userType}</p>
                )}
              </div>
            </div>

            {/* Action Buttons */}
            <div className="mt-8 flex flex-wrap gap-3">
              <Button
                onClick={handleSubmit}
                disabled={loading}
                className="bg-blue-600 hover:bg-blue-700 text-white"
              >
                {loading ? (
                  <div className="flex items-center">
                    <div className="animate-spin rounded-full h-4 w-4 border-b-2 border-white mr-2"></div>
                    Processing...
                  </div>
                ) : (
                  'Enter - Add User'
                )}
              </Button>
              
              <Button
                onClick={handleSaveAndExit}
                disabled={loading}
                className="bg-green-600 hover:bg-green-700 text-white"
              >
                PF3 Save & Exit
              </Button>
              
              <Button
                onClick={handleClearAll}
                disabled={loading}
                variant="outline"
              >
                PF4 Clear All
              </Button>
              
              <Button
                onClick={handleCancel}
                disabled={loading}
                variant="outline"
              >
                PF12 Cancel
              </Button>
            </div>
          </div>

          {/* Instructions */}
          <div className="mt-6 bg-gray-100 rounded-lg p-4">
            <h3 className="text-sm font-semibold text-gray-700 mb-2">Instructions:</h3>
            <div className="text-xs text-gray-600 space-y-1">
              <p>• All fields marked with (*) are mandatory and must be completed</p>
              <p>• User ID must be unique and contain only uppercase letters and numbers</p>
              <p>• Password must be between 4 and 8 characters long</p>
              <p>• Press Enter or click "Add User" to validate and save the user</p>
              <p>• Press F3 or click "Save & Exit" to save and return to admin menu</p>
              <p>• Press F4 or click "Clear All" to clear all form fields</p>
              <p>• Press F12 or click "Cancel" to exit without saving</p>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
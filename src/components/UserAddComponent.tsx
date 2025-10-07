'use client';

import React, { useState, useRef, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { userSecurityService } from '@/services/userSecurityService';
import { CreateUserSecurityRequest, ValidationError, UserSecurityActionResult } from '@/types/userSecurity';

interface UserAddComponentProps {
  onUserCreated?: (userId: string) => void;
  onExit?: () => void;
  className?: string;
}

const UserAddComponent: React.FC<UserAddComponentProps> = ({
  onUserCreated,
  onExit,
  className = ''
}) => {
  const router = useRouter();
  const [formData, setFormData] = useState<CreateUserSecurityRequest>({
    userId: '',
    userName: '',
    userType: 'GENERAL',
    email: '',
    firstName: '',
    lastName: '',
    password: '',
    confirmPassword: '',
    isActive: true
  });

  const [errors, setErrors] = useState<Record<string, string>>({});
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const [successMessage, setSuccessMessage] = useState<string>('');

  const firstNameRef = useRef<HTMLInputElement>(null);

  useEffect(() => {
    if (firstNameRef.current) {
      firstNameRef.current.focus();
    }
  }, []);

  const clearErrors = () => {
    setErrors({});
    setSuccessMessage('');
  };

  const validateForm = (): boolean => {
    const newErrors: Record<string, string> = {};

    // First Name validation (COUSR01C business rule)
    if (!formData.firstName.trim()) {
      newErrors.firstName = 'First Name can NOT be empty...';
    }

    // Last Name validation (COUSR01C business rule)
    if (!formData.lastName.trim()) {
      newErrors.lastName = 'Last Name can NOT be empty...';
    }

    // User ID validation (COUSR01C business rule)
    if (!formData.userId.trim()) {
      newErrors.userId = 'User ID can NOT be empty...';
    } else if (formData.userId.length > 8) {
      newErrors.userId = 'User ID cannot exceed 8 characters';
    }

    // Password validation (COUSR01C business rule)
    if (!formData.password) {
      newErrors.password = 'Password can NOT be empty...';
    } else if (formData.password.length > 8) {
      newErrors.password = 'Password cannot exceed 8 characters';
    }

    // User Type validation (COUSR01C business rule)
    if (!formData.userType) {
      newErrors.userType = 'User Type can NOT be empty...';
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleInputChange = (field: keyof CreateUserSecurityRequest, value: string) => {
    setFormData(prev => ({
      ...prev,
      [field]: value
    }));

    // Clear field-specific error when user starts typing
    if (errors[field]) {
      setErrors(prev => ({
        ...prev,
        [field]: ''
      }));
    }

    // Clear success message when form is modified
    if (successMessage) {
      setSuccessMessage('');
    }
  };

  const handleSubmit = async () => {
    if (!validateForm()) {
      return;
    }

    setIsLoading(true);
    clearErrors();

    try {
      // Set userName to be the same as userId for COUSR01C business logic
      const requestData: CreateUserSecurityRequest = {
        ...formData,
        userName: formData.userId,
        email: formData.email || `${formData.userId}@company.com` // Generate email if not provided
      };

      const result: UserSecurityActionResult = await userSecurityService.createUserWithResult(requestData);

      if (result.success) {
        // Success message with green color (COUSR01C business rule)
        setSuccessMessage(`User ${formData.userId} has been added ...`);
        
        // Clear form after successful creation (COUSR01C business rule)
        setFormData({
          userId: '',
          userName: '',
          userType: 'GENERAL',
          email: '',
          firstName: '',
          lastName: '',
          password: '',
          confirmPassword: '',
          isActive: true
        });

        // Focus back to first field (COUSR01C business rule)
        if (firstNameRef.current) {
          firstNameRef.current.focus();
        }

        // Notify parent component
        if (onUserCreated && result.data) {
          onUserCreated(result.data.userId);
        }
      } else {
        // Check for duplicate user error (COUSR01C business rule)
        if (result.message.toLowerCase().includes('already exist')) {
          setErrors({ userId: 'User ID already exist...' });
        } else {
          // Handle other errors
          setErrors({ general: result.message || 'Unable to Add User...' });
        }
      }
    } catch (error) {
      console.error('Error creating user:', error);
      setErrors({ general: 'Unable to Add User...' });
    } finally {
      setIsLoading(false);
    }
  };

  const handleClear = () => {
    setFormData({
      userId: '',
      userName: '',
      userType: 'GENERAL',
      email: '',
      firstName: '',
      lastName: '',
      password: '',
      confirmPassword: '',
      isActive: true
    });
    clearErrors();

    if (firstNameRef.current) {
      firstNameRef.current.focus();
    }
  };

  const handleExit = () => {
    if (onExit) {
      onExit();
    } else {
      router.push('/admin/dashboard');
    }
  };

  const handleKeyDown = (event: React.KeyboardEvent) => {
    switch (event.key) {
      case 'Enter':
        event.preventDefault();
        if (!isLoading) {
          handleSubmit();
        }
        break;

      case 'F3':
        event.preventDefault();
        if (!isLoading) {
          handleExit();
        }
        break;

      case 'F4':
        event.preventDefault();
        if (!isLoading) {
          handleClear();
        }
        break;

      default:
        // Allow normal key processing
        break;
    }
  };

  return (
    <div 
      className={`min-h-screen bg-gray-50 p-6 ${className}`}
      onKeyDown={handleKeyDown}
      tabIndex={-1}
    >
      <div className="max-w-4xl mx-auto">
        {/* Header */}
        <div className="mb-6">
          <h1 className="text-2xl font-bold text-gray-900 mb-2">
            CardDemo
          </h1>
          <h2 className="text-xl text-gray-700 mb-2">
            User Add
          </h2>
          <div className="flex justify-between text-sm text-gray-600">
            <span>COUSR01C - CU01</span>
            <span>{new Date().toLocaleDateString()} {new Date().toLocaleTimeString()}</span>
          </div>
        </div>

        {/* Success Message */}
        {successMessage && (
          <div className="bg-green-50 border border-green-200 rounded-md p-4 mb-6">
            <p className="text-sm text-green-800 font-medium">{successMessage}</p>
          </div>
        )}

        {/* General Error */}
        {errors.general && (
          <div className="bg-red-50 border border-red-200 rounded-md p-4 mb-6">
            <p className="text-sm text-red-800">{errors.general}</p>
          </div>
        )}

        {/* User Form */}
        <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-6 mb-6">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            {/* First Name */}
            <div>
              <label htmlFor="firstName" className="block text-sm font-medium text-gray-700 mb-1">
                First Name
              </label>
              <input
                ref={firstNameRef}
                id="firstName"
                type="text"
                value={formData.firstName}
                onChange={(e) => handleInputChange('firstName', e.target.value)}
                placeholder="Enter first name"
                className={`w-full px-3 py-2 border rounded-md focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 ${
                  errors.firstName ? 'border-red-500' : 'border-gray-300'
                }`}
                disabled={isLoading}
                maxLength={20}
              />
              {errors.firstName && (
                <p className="mt-1 text-sm text-red-600">{errors.firstName}</p>
              )}
            </div>

            {/* Last Name */}
            <div>
              <label htmlFor="lastName" className="block text-sm font-medium text-gray-700 mb-1">
                Last Name
              </label>
              <input
                id="lastName"
                type="text"
                value={formData.lastName}
                onChange={(e) => handleInputChange('lastName', e.target.value)}
                placeholder="Enter last name"
                className={`w-full px-3 py-2 border rounded-md focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 ${
                  errors.lastName ? 'border-red-500' : 'border-gray-300'
                }`}
                disabled={isLoading}
                maxLength={20}
              />
              {errors.lastName && (
                <p className="mt-1 text-sm text-red-600">{errors.lastName}</p>
              )}
            </div>

            {/* User ID */}
            <div>
              <label htmlFor="userId" className="block text-sm font-medium text-gray-700 mb-1">
                User ID
              </label>
              <input
                id="userId"
                type="text"
                value={formData.userId}
                onChange={(e) => handleInputChange('userId', e.target.value.toUpperCase())}
                placeholder="Enter user ID (max 8 chars)"
                className={`w-full px-3 py-2 border rounded-md focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 ${
                  errors.userId ? 'border-red-500' : 'border-gray-300'
                }`}
                disabled={isLoading}
                maxLength={8}
              />
              {errors.userId && (
                <p className="mt-1 text-sm text-red-600">{errors.userId}</p>
              )}
            </div>

            {/* Password */}
            <div>
              <label htmlFor="password" className="block text-sm font-medium text-gray-700 mb-1">
                Password
              </label>
              <input
                id="password"
                type="password"
                value={formData.password}
                onChange={(e) => handleInputChange('password', e.target.value)}
                placeholder="Enter password (max 8 chars)"
                className={`w-full px-3 py-2 border rounded-md focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 ${
                  errors.password ? 'border-red-500' : 'border-gray-300'
                }`}
                disabled={isLoading}
                maxLength={8}
              />
              {errors.password && (
                <p className="mt-1 text-sm text-red-600">{errors.password}</p>
              )}
            </div>

            {/* User Type */}
            <div className="md:col-span-2">
              <label htmlFor="userType" className="block text-sm font-medium text-gray-700 mb-1">
                User Type
              </label>
              <select
                id="userType"
                value={formData.userType}
                onChange={(e) => handleInputChange('userType', e.target.value as 'ADMIN' | 'GENERAL')}
                className={`w-full px-3 py-2 border rounded-md focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 ${
                  errors.userType ? 'border-red-500' : 'border-gray-300'
                }`}
                disabled={isLoading}
              >
                <option value="GENERAL">General User</option>
                <option value="ADMIN">Administrator</option>
              </select>
              {errors.userType && (
                <p className="mt-1 text-sm text-red-600">{errors.userType}</p>
              )}
            </div>
          </div>
        </div>

        {/* Action Buttons */}
        <div className="flex flex-wrap gap-4 mb-6">
          <button
            onClick={handleSubmit}
            disabled={isLoading}
            className="flex-1 sm:flex-none min-w-[120px] bg-indigo-600 text-white px-4 py-2 rounded-md hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-indigo-500 disabled:opacity-50 disabled:cursor-not-allowed"
          >
            {isLoading ? 'Adding...' : 'Add User (Enter)'}
          </button>
          
          <button
            onClick={handleClear}
            disabled={isLoading}
            className="flex-1 sm:flex-none min-w-[120px] bg-gray-600 text-white px-4 py-2 rounded-md hover:bg-gray-700 focus:outline-none focus:ring-2 focus:ring-gray-500 disabled:opacity-50 disabled:cursor-not-allowed"
          >
            Clear (F4)
          </button>
          
          <button
            onClick={handleExit}
            disabled={isLoading}
            className="flex-1 sm:flex-none min-w-[120px] bg-gray-600 text-white px-4 py-2 rounded-md hover:bg-gray-700 focus:outline-none focus:ring-2 focus:ring-gray-500 disabled:opacity-50 disabled:cursor-not-allowed"
          >
            Exit (F3)
          </button>
        </div>

        {/* Function Key Instructions */}
        <div className="bg-gray-50 border border-gray-200 rounded-md p-4">
          <h3 className="text-sm font-medium text-gray-900 mb-2">Instructions</h3>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-2 text-xs text-gray-600">
            <div><span className="font-medium">Enter:</span> Validate and add user</div>
            <div><span className="font-medium">F3:</span> Exit to Admin Menu</div>
            <div><span className="font-medium">F4:</span> Clear all fields</div>
          </div>
          <div className="mt-2 text-xs text-gray-500">
            All fields are mandatory. User ID and Password limited to 8 characters.
          </div>
        </div>
      </div>
    </div>
  );
};

export default UserAddComponent;
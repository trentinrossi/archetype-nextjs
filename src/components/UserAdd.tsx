'use client';

import React, { useState, useEffect, useCallback } from 'react';
import { User, CreateUserRequest, APIResponse, UserValidationResult } from '@/types/user';
import { userService } from '@/services/userService';
import { Button } from '@/components/ui/Button';
import { Input } from '@/components/ui/Input';
import { Select } from '@/components/ui/Select';

interface UserAddProps {
  onUserCreated?: (user: User) => void;
  onExit?: () => void;
  className?: string;
}

export const UserAdd: React.FC<UserAddProps> = ({
  onUserCreated,
  onExit,
  className = '',
}) => {
  const [formData, setFormData] = useState<CreateUserRequest & { userId: string }>({
    userId: '',
    firstName: '',
    lastName: '',
    password: '',
    userType: 'U',
  });

  const [errors, setErrors] = useState<Record<string, string>>({});
  const [loading, setLoading] = useState(false);
  const [successMessage, setSuccessMessage] = useState<string>('');
  const [generalError, setGeneralError] = useState<string>('');

  const clearMessages = useCallback(() => {
    setSuccessMessage('');
    setGeneralError('');
    setErrors({});
  }, []);

  const validateForm = useCallback((): boolean => {
    const newErrors: Record<string, string> = {};

    // Validate User ID
    if (!formData.userId.trim()) {
      newErrors.userId = 'User ID is required';
    } else if (formData.userId.length !== 8) {
      newErrors.userId = 'User ID must be exactly 8 characters';
    } else if (!/^[A-Za-z0-9]+$/.test(formData.userId)) {
      newErrors.userId = 'User ID must contain only alphanumeric characters';
    }

    // Validate First Name
    if (!formData.firstName.trim()) {
      newErrors.firstName = 'First Name is required';
    } else if (formData.firstName.length > 20) {
      newErrors.firstName = 'First Name cannot exceed 20 characters';
    }

    // Validate Last Name
    if (!formData.lastName.trim()) {
      newErrors.lastName = 'Last Name is required';
    } else if (formData.lastName.length > 20) {
      newErrors.lastName = 'Last Name cannot exceed 20 characters';
    }

    // Validate Password
    if (!formData.password) {
      newErrors.password = 'Password is required';
    } else if (formData.password.length !== 8) {
      newErrors.password = 'Password must be exactly 8 characters';
    }

    // Validate User Type
    if (!formData.userType) {
      newErrors.userType = 'User Type is required';
    } else if (!['A', 'U'].includes(formData.userType)) {
      newErrors.userType = 'User Type must be A (Admin) or U (User)';
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  }, [formData]);

  const handleInputChange = useCallback((field: keyof typeof formData, value: string) => {
    clearMessages();
    
    let processedValue = value;
    
    // Apply field-specific processing
    switch (field) {
      case 'userId':
        processedValue = value.toUpperCase().slice(0, 8);
        break;
      case 'firstName':
      case 'lastName':
        processedValue = value.slice(0, 20);
        break;
      case 'password':
        processedValue = value.slice(0, 8);
        break;
      case 'userType':
        processedValue = value as 'A' | 'U';
        break;
    }

    setFormData(prev => ({
      ...prev,
      [field]: processedValue,
    }));

    // Clear field-specific error when user starts typing
    if (errors[field]) {
      setErrors(prev => {
        const newErrors = { ...prev };
        delete newErrors[field];
        return newErrors;
      });
    }
  }, [errors, clearMessages]);

  const handleSubmit = useCallback(async () => {
    clearMessages();

    if (!validateForm()) {
      setGeneralError('Please correct the validation errors above');
      return;
    }

    setLoading(true);

    try {
      const createData: CreateUserRequest = {
        firstName: formData.firstName.trim(),
        lastName: formData.lastName.trim(),
        password: formData.password,
        userType: formData.userType,
      };

      // First validate with service
      const validation: UserValidationResult = userService.validateUser({
        userId: formData.userId,
        ...createData,
      });

      if (!validation.isValid) {
        const validationErrors: Record<string, string> = {};
        validation.errors.forEach(error => {
          validationErrors[error.field] = error.message;
        });
        setErrors(validationErrors);
        setGeneralError('Please correct the validation errors');
        return;
      }

      // Attempt to create user
      const response: APIResponse<User> = await userService.createUser(createData);

      if (response.success && response.data) {
        setSuccessMessage(`User ${formData.userId} created successfully!`);
        
        if (onUserCreated) {
          onUserCreated(response.data);
        }

        // Clear form after successful creation
        setTimeout(() => {
          handleClear();
        }, 2000);
      } else {
        // Handle API errors
        if (response.error?.statusCode === 409) {
          setErrors({ userId: 'User ID already exists' });
          setGeneralError('User ID already exists. Please choose a different User ID.');
        } else if (response.error?.statusCode === 400) {
          setGeneralError(response.error.message || 'Invalid user data provided');
        } else {
          setGeneralError(response.error?.message || 'Failed to create user. Please try again.');
        }
      }
    } catch (error) {
      setGeneralError('System error occurred. Please try again later.');
      console.error('User creation error:', error);
    } finally {
      setLoading(false);
    }
  }, [formData, validateForm, clearMessages, onUserCreated]);

  const handleClear = useCallback(() => {
    setFormData({
      userId: '',
      firstName: '',
      lastName: '',
      password: '',
      userType: 'U',
    });
    setErrors({});
    clearMessages();
  }, [clearMessages]);

  const handleExit = useCallback(() => {
    if (onExit) {
      onExit();
    }
  }, [onExit]);

  const generateUserId = useCallback(() => {
    const generatedId = userService.generateUserId();
    handleInputChange('userId', generatedId);
  }, [handleInputChange]);

  const generatePassword = useCallback(() => {
    const generatedPassword = userService.generatePassword();
    handleInputChange('password', generatedPassword);
  }, [handleInputChange]);

  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      switch (event.key) {
        case 'Enter':
          event.preventDefault();
          if (!loading) {
            handleSubmit();
          }
          break;
        case 'F3':
          event.preventDefault();
          handleExit();
          break;
        case 'F4':
          event.preventDefault();
          handleClear();
          break;
        case 'Escape':
          event.preventDefault();
          handleExit();
          break;
      }
    };

    document.addEventListener('keydown', handleKeyDown);
    return () => {
      document.removeEventListener('keydown', handleKeyDown);
    };
  }, [loading, handleSubmit, handleExit, handleClear]);

  const userTypeOptions = [
    { value: 'U', label: 'U - User' },
    { value: 'A', label: 'A - Admin' },
  ];

  return (
    <div className={`bg-background border border-border rounded-lg p-6 max-w-2xl mx-auto ${className}`}>
      {/* Header */}
      <div className="mb-6">
        <h1 className="text-2xl font-bold text-foreground mb-2">Add User - COUSR01C</h1>
        <p className="text-sm text-muted-foreground">
          Use ENTER to submit, PF3 to exit, PF4 to clear form
        </p>
      </div>

      {/* Success Message */}
      {successMessage && (
        <div className="mb-4 p-3 bg-green-50 border border-green-200 rounded-md">
          <p className="text-green-800 text-sm font-medium">{successMessage}</p>
        </div>
      )}

      {/* General Error Message */}
      {generalError && (
        <div className="mb-4 p-3 bg-destructive/10 border border-destructive/20 rounded-md">
          <p className="text-destructive text-sm">{generalError}</p>
        </div>
      )}

      {/* Form */}
      <div className="space-y-4">
        {/* User ID */}
        <div className="flex gap-2">
          <div className="flex-1">
            <Input
              label="User ID *"
              value={formData.userId}
              onChange={(e) => handleInputChange('userId', e.target.value)}
              error={errors.userId}
              maxLength={8}
              placeholder="8 characters"
              className="font-mono"
              disabled={loading}
            />
          </div>
          <div className="flex items-end">
            <Button
              onClick={generateUserId}
              variant="outline"
              size="sm"
              disabled={loading}
              className="mb-1"
            >
              Generate
            </Button>
          </div>
        </div>

        {/* First Name */}
        <Input
          label="First Name *"
          value={formData.firstName}
          onChange={(e) => handleInputChange('firstName', e.target.value)}
          error={errors.firstName}
          maxLength={20}
          placeholder="Maximum 20 characters"
          disabled={loading}
        />

        {/* Last Name */}
        <Input
          label="Last Name *"
          value={formData.lastName}
          onChange={(e) => handleInputChange('lastName', e.target.value)}
          error={errors.lastName}
          maxLength={20}
          placeholder="Maximum 20 characters"
          disabled={loading}
        />

        {/* Password */}
        <div className="flex gap-2">
          <div className="flex-1">
            <Input
              label="Password *"
              type="password"
              value={formData.password}
              onChange={(e) => handleInputChange('password', e.target.value)}
              error={errors.password}
              maxLength={8}
              placeholder="8 characters"
              className="font-mono"
              disabled={loading}
            />
          </div>
          <div className="flex items-end">
            <Button
              onClick={generatePassword}
              variant="outline"
              size="sm"
              disabled={loading}
              className="mb-1"
            >
              Generate
            </Button>
          </div>
        </div>

        {/* User Type */}
        <Select
          label="User Type *"
          value={formData.userType}
          onChange={(value) => handleInputChange('userType', value)}
          options={userTypeOptions}
          error={errors.userType}
          disabled={loading}
        />
      </div>

      {/* Character Counters */}
      <div className="mt-4 text-xs text-muted-foreground space-y-1">
        <div className="flex justify-between">
          <span>User ID: {formData.userId.length}/8</span>
          <span>First Name: {formData.firstName.length}/20</span>
        </div>
        <div className="flex justify-between">
          <span>Last Name: {formData.lastName.length}/20</span>
          <span>Password: {formData.password.length}/8</span>
        </div>
      </div>

      {/* Action Buttons */}
      <div className="flex justify-between items-center mt-6 pt-4 border-t border-border">
        <div className="flex gap-2">
          <Button
            onClick={handleSubmit}
            disabled={loading}
            variant="primary"
          >
            {loading ? 'Creating...' : 'Create User (ENTER)'}
          </Button>
          <Button
            onClick={handleClear}
            disabled={loading}
            variant="outline"
          >
            Clear (PF4)
          </Button>
        </div>

        <Button
          onClick={handleExit}
          disabled={loading}
          variant="outline"
        >
          Exit (PF3)
        </Button>
      </div>

      {/* Keyboard Shortcuts Help */}
      <div className="mt-4 pt-4 border-t border-border">
        <p className="text-xs text-muted-foreground">
          <strong>Keyboard Shortcuts:</strong> ENTER = Submit | PF3 = Exit | PF4 = Clear | ESC = Exit
        </p>
      </div>

      {/* Field Requirements */}
      <div className="mt-4 p-3 bg-muted/30 rounded-md">
        <p className="text-xs text-muted-foreground mb-2"><strong>Field Requirements:</strong></p>
        <ul className="text-xs text-muted-foreground space-y-1">
          <li>• User ID: Exactly 8 alphanumeric characters (required)</li>
          <li>• First Name: Maximum 20 characters (required)</li>
          <li>• Last Name: Maximum 20 characters (required)</li>
          <li>• Password: Exactly 8 characters (required)</li>
          <li>• User Type: A = Admin, U = User (required)</li>
        </ul>
      </div>
    </div>
  );
};

export default UserAdd;
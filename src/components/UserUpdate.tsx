'use client';

import React, { useState, useEffect, useCallback } from 'react';
import { User, UpdateUserRequest, APIResponse, UserValidationResult } from '@/types/user';
import { userService } from '@/services/userService';
import { Button } from '@/components/ui/Button';
import { Input } from '@/components/ui/Input';
import { Select } from '@/components/ui/Select';

interface UserUpdateProps {
  onUserUpdated?: (user: User) => void;
  onExit?: () => void;
  className?: string;
}

export const UserUpdate: React.FC<UserUpdateProps> = ({
  onUserUpdated,
  onExit,
  className = '',
}) => {
  const [lookupUserId, setLookupUserId] = useState<string>('');
  const [currentUser, setCurrentUser] = useState<User | null>(null);
  const [formData, setFormData] = useState<UpdateUserRequest>({
    firstName: '',
    lastName: '',
    password: '',
    userType: 'U',
  });
  const [originalData, setOriginalData] = useState<UpdateUserRequest>({});

  const [errors, setErrors] = useState<Record<string, string>>({});
  const [loading, setLoading] = useState(false);
  const [lookupLoading, setLookupLoading] = useState(false);
  const [successMessage, setSuccessMessage] = useState<string>('');
  const [generalError, setGeneralError] = useState<string>('');
  const [isEditMode, setIsEditMode] = useState(false);

  const clearMessages = useCallback(() => {
    setSuccessMessage('');
    setGeneralError('');
    setErrors({});
  }, []);

  const validateForm = useCallback((): boolean => {
    const newErrors: Record<string, string> = {};

    // Validate First Name
    if (!formData.firstName?.trim()) {
      newErrors.firstName = 'First Name is required';
    } else if (formData.firstName.length > 20) {
      newErrors.firstName = 'First Name cannot exceed 20 characters';
    }

    // Validate Last Name
    if (!formData.lastName?.trim()) {
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

  const hasChanges = useCallback((): boolean => {
    return (
      formData.firstName !== originalData.firstName ||
      formData.lastName !== originalData.lastName ||
      formData.password !== originalData.password ||
      formData.userType !== originalData.userType
    );
  }, [formData, originalData]);

  const handleLookupUserIdChange = useCallback((value: string) => {
    clearMessages();
    setLookupUserId(value.toUpperCase().slice(0, 8));
  }, [clearMessages]);

  const handleInputChange = useCallback((field: keyof UpdateUserRequest, value: string) => {
    clearMessages();
    
    let processedValue = value;
    
    // Apply field-specific processing
    switch (field) {
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

  const handleLookupUser = useCallback(async () => {
    if (!lookupUserId.trim()) {
      setGeneralError('Please enter a User ID to lookup');
      return;
    }

    if (lookupUserId.length !== 8) {
      setGeneralError('User ID must be exactly 8 characters');
      return;
    }

    clearMessages();
    setLookupLoading(true);
    setCurrentUser(null);
    setIsEditMode(false);

    try {
      const response: APIResponse<User> = await userService.getUserById(lookupUserId);

      if (response.success && response.data) {
        const user = response.data;
        setCurrentUser(user);
        
        const userData: UpdateUserRequest = {
          firstName: user.firstName,
          lastName: user.lastName,
          password: user.password,
          userType: user.userType,
        };
        
        setFormData(userData);
        setOriginalData(userData);
        setIsEditMode(true);
        setSuccessMessage(`User ${user.userId} found and loaded for editing`);
      } else {
        // Handle API errors
        if (response.error?.statusCode === 404) {
          setGeneralError(`User ${lookupUserId} not found`);
        } else {
          setGeneralError(response.error?.message || 'Failed to lookup user. Please try again.');
        }
      }
    } catch (error) {
      setGeneralError('System error occurred during lookup. Please try again later.');
      console.error('User lookup error:', error);
    } finally {
      setLookupLoading(false);
    }
  }, [lookupUserId, clearMessages]);

  const handleUpdate = useCallback(async () => {
    if (!currentUser) {
      setGeneralError('No user loaded for update');
      return;
    }

    clearMessages();

    if (!validateForm()) {
      setGeneralError('Please correct the validation errors above');
      return;
    }

    if (!hasChanges()) {
      setGeneralError('No changes made to update');
      return;
    }

    setLoading(true);

    try {
      const updateData: UpdateUserRequest = {
        firstName: formData.firstName?.trim(),
        lastName: formData.lastName?.trim(),
        password: formData.password,
        userType: formData.userType,
      };

      // First validate with service
      const validation: UserValidationResult = userService.validateUser({
        userId: currentUser.userId,
        ...updateData,
      } as User);

      if (!validation.isValid) {
        const validationErrors: Record<string, string> = {};
        validation.errors.forEach(error => {
          validationErrors[error.field] = error.message;
        });
        setErrors(validationErrors);
        setGeneralError('Please correct the validation errors');
        return;
      }

      // Attempt to update user
      const response: APIResponse<User> = await userService.updateUser(currentUser.userId, updateData);

      if (response.success && response.data) {
        const updatedUser = response.data;
        setCurrentUser(updatedUser);
        setOriginalData(updateData);
        setSuccessMessage(`User ${currentUser.userId} updated successfully!`);
        
        if (onUserUpdated) {
          onUserUpdated(updatedUser);
        }
      } else {
        // Handle API errors
        if (response.error?.statusCode === 404) {
          setGeneralError('User not found for update');
        } else if (response.error?.statusCode === 400) {
          setGeneralError(response.error.message || 'Invalid user data provided');
        } else {
          setGeneralError(response.error?.message || 'Failed to update user. Please try again.');
        }
      }
    } catch (error) {
      setGeneralError('System error occurred. Please try again later.');
      console.error('User update error:', error);
    } finally {
      setLoading(false);
    }
  }, [currentUser, formData, validateForm, hasChanges, clearMessages, onUserUpdated, originalData]);

  const handleClear = useCallback(() => {
    if (currentUser && originalData) {
      setFormData({ ...originalData });
    } else {
      setFormData({
        firstName: '',
        lastName: '',
        password: '',
        userType: 'U',
      });
    }
    setErrors({});
    clearMessages();
  }, [currentUser, originalData, clearMessages]);

  const handleSaveAndExit = useCallback(async () => {
    if (currentUser && hasChanges()) {
      await handleUpdate();
      // Exit after successful update
      setTimeout(() => {
        if (onExit) {
          onExit();
        }
      }, 1500);
    } else {
      if (onExit) {
        onExit();
      }
    }
  }, [currentUser, hasChanges, handleUpdate, onExit]);

  const handleExit = useCallback(() => {
    if (onExit) {
      onExit();
    }
  }, [onExit]);

  const generatePassword = useCallback(() => {
    const generatedPassword = userService.generatePassword();
    handleInputChange('password', generatedPassword);
  }, [handleInputChange]);

  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      switch (event.key) {
        case 'Enter':
          event.preventDefault();
          if (!lookupLoading && !loading) {
            if (!isEditMode) {
              handleLookupUser();
            } else {
              handleUpdate();
            }
          }
          break;
        case 'F3':
          event.preventDefault();
          handleSaveAndExit();
          break;
        case 'F4':
          event.preventDefault();
          handleClear();
          break;
        case 'F5':
          event.preventDefault();
          if (isEditMode) {
            handleUpdate();
          }
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
  }, [lookupLoading, loading, isEditMode, handleLookupUser, handleUpdate, handleSaveAndExit, handleClear, handleExit]);

  const userTypeOptions = [
    { value: 'U', label: 'U - User' },
    { value: 'A', label: 'A - Admin' },
  ];

  return (
    <div className={`bg-background border border-border rounded-lg p-6 max-w-2xl mx-auto ${className}`}>
      {/* Header */}
      <div className="mb-6">
        <h1 className="text-2xl font-bold text-foreground mb-2">Update User - COUSR02C</h1>
        <p className="text-sm text-muted-foreground">
          {!isEditMode 
            ? 'Enter User ID and press ENTER to lookup user for editing'
            : 'Use ENTER to update, PF3 to save & exit, PF4 to clear, PF5 to update'
          }
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

      {/* User Lookup Section */}
      {!isEditMode && (
        <div className="mb-6 p-4 bg-muted/30 rounded-md">
          <div className="flex gap-2">
            <div className="flex-1">
              <Input
                label="User ID to Update *"
                value={lookupUserId}
                onChange={(e) => handleLookupUserIdChange(e.target.value)}
                maxLength={8}
                placeholder="Enter 8-character User ID"
                className="font-mono"
                disabled={lookupLoading}
              />
            </div>
            <div className="flex items-end">
              <Button
                onClick={handleLookupUser}
                disabled={lookupLoading || !lookupUserId.trim()}
                variant="primary"
                className="mb-1"
              >
                {lookupLoading ? 'Looking up...' : 'Lookup (ENTER)'}
              </Button>
            </div>
          </div>
          <p className="text-xs text-muted-foreground mt-2">
            User ID: {lookupUserId.length}/8 characters
          </p>
        </div>
      )}

      {/* Current User Info */}
      {currentUser && (
        <div className="mb-4 p-3 bg-primary/5 border border-primary/20 rounded-md">
          <p className="text-sm font-medium text-primary">
            Editing User: <span className="font-mono">{currentUser.userId}</span>
          </p>
          {currentUser.createdAt && (
            <p className="text-xs text-muted-foreground">
              Created: {new Date(currentUser.createdAt).toLocaleString()}
            </p>
          )}
        </div>
      )}

      {/* Form - Only show when in edit mode */}
      {isEditMode && (
        <>
          <div className="space-y-4">
            {/* User ID Display (Non-editable) */}
            <div>
              <label className="block text-sm font-medium mb-2">User ID</label>
              <div className="flex h-10 w-full rounded-md border border-border bg-muted px-3 py-2 text-sm font-mono text-muted-foreground">
                {currentUser?.userId}
              </div>
              <p className="text-xs text-muted-foreground mt-1">User ID cannot be changed</p>
            </div>

            {/* First Name */}
            <Input
              label="First Name *"
              value={formData.firstName || ''}
              onChange={(e) => handleInputChange('firstName', e.target.value)}
              error={errors.firstName}
              maxLength={20}
              placeholder="Maximum 20 characters"
              disabled={loading}
            />

            {/* Last Name */}
            <Input
              label="Last Name *"
              value={formData.lastName || ''}
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
                  value={formData.password || ''}
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
              value={formData.userType || 'U'}
              onChange={(value) => handleInputChange('userType', value)}
              options={userTypeOptions}
              error={errors.userType}
              disabled={loading}
            />
          </div>

          {/* Character Counters */}
          <div className="mt-4 text-xs text-muted-foreground space-y-1">
            <div className="flex justify-between">
              <span>First Name: {(formData.firstName || '').length}/20</span>
              <span>Last Name: {(formData.lastName || '').length}/20</span>
            </div>
            <div className="flex justify-between">
              <span>Password: {(formData.password || '').length}/8</span>
              <span className={hasChanges() ? 'text-orange-600 font-medium' : ''}>
                {hasChanges() ? 'Changes detected' : 'No changes'}
              </span>
            </div>
          </div>

          {/* Action Buttons */}
          <div className="flex justify-between items-center mt-6 pt-4 border-t border-border">
            <div className="flex gap-2">
              <Button
                onClick={handleUpdate}
                disabled={loading || !hasChanges()}
                variant="primary"
              >
                {loading ? 'Updating...' : 'Update (ENTER/PF5)'}
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
              onClick={handleSaveAndExit}
              disabled={loading}
              variant="outline"
            >
              Save & Exit (PF3)
            </Button>
          </div>
        </>
      )}

      {/* Exit Button - Always visible */}
      {!isEditMode && (
        <div className="flex justify-end mt-6 pt-4 border-t border-border">
          <Button
            onClick={handleExit}
            disabled={lookupLoading}
            variant="outline"
          >
            Exit (PF3)
          </Button>
        </div>
      )}

      {/* Keyboard Shortcuts Help */}
      <div className="mt-4 pt-4 border-t border-border">
        <p className="text-xs text-muted-foreground">
          <strong>Keyboard Shortcuts:</strong> 
          {!isEditMode 
            ? ' ENTER = Lookup User | PF3 = Exit | ESC = Exit'
            : ' ENTER = Update | PF3 = Save & Exit | PF4 = Clear | PF5 = Update | ESC = Exit'
          }
        </p>
      </div>

      {/* Field Requirements */}
      {isEditMode && (
        <div className="mt-4 p-3 bg-muted/30 rounded-md">
          <p className="text-xs text-muted-foreground mb-2"><strong>Field Requirements:</strong></p>
          <ul className="text-xs text-muted-foreground space-y-1">
            <li>• User ID: Cannot be changed (system assigned)</li>
            <li>• First Name: Maximum 20 characters (required)</li>
            <li>• Last Name: Maximum 20 characters (required)</li>
            <li>• Password: Exactly 8 characters (required)</li>
            <li>• User Type: A = Admin, U = User (required)</li>
          </ul>
        </div>
      )}
    </div>
  );
};

export default UserUpdate;